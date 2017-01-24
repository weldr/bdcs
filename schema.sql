-- This describes the schema used by the metadata database (mddb).  We
-- considered several options for how to implement the mddb, finally deciding
-- upon sqlite (though, other relational databases would also be fine).  We
-- would like the mddb to eventually hold the data for multiple builds of each
-- package from many releases.  This could mean hundreds of millions of rows
-- needed to store the files.  Thus, the ability to work with a fairly large
-- database is important.
--
-- We evaluated document-based databases like mongodb but found it too slow and
-- a little unwieldy to use for our purposes.  We also evaluated key/value
-- systems like redis but found that accessing data was slow unless you add
-- your own index algorithm.  And if you're going to do that, you might as well
-- use something that already provides indexing.  Add sqlite's ubiquity and
-- the ability to move to another relational database if needed, and the
-- decision was made.
--
-- In general, the design of this database does not worry too much about
-- normal forms.  However, we do use a lot of intermediate tables to represent
-- one-to-many relationships as well as ensure that a piece of data only exists
-- in one place in the database.  Examples of these intermediate tables are
-- the various *_files tables and the *_key_values tables.
--
-- At the same time, we have sought to not overload the database with tons of
-- unnecessary tables.  That is why there is the key_val table and all the
-- associated *_key_values tables.  There are lots of pieces of data at all
-- levels (builds, files, etc.) that only exist for a handful of items.
-- Alternately, there are pieces of data that exist with a wide range of
-- possible values.  The generic key_val table allows storing this kind of
-- data without making too much of a mess.

-- A project is the database's representation of some upstream that produces
-- a piece of software.  This could be as simple as a tarball that gets built
-- into a single RPM, or as complicated as a live OS image, or anything in
-- between.  We don't impose any restrictions here on what kind of thing a
-- project can be.
--
-- Starting with this table, we split NEVRA-style information up between here,
-- sources, and builds because it makes sense to do so.  At each level, we only
-- store information that does not change between instances of that level.  For
-- instance, this table stores the name.  All sources released from a given
-- project will have the same name, as will all builds created from those
-- sources.
--
-- At any one time, there will only be one row in this table for a single
-- project.  It does not make sense to have several instances.
create table projects (
    id integer primary key,
    name text not null unique,
    summary text not null,
    description text not null,
    homepage text,
    upstream_vcs text not null
);

-- A source represents a release of a single upstream project (hence the
-- project_id reference).  This table continues the theme of spreading NEVRA
-- style information out to several tables.  In this table we store the version
-- since that piece is specific to a source.
--
-- Over time, there will be several entries in this table with the same
-- project_id, as an upstream makes several releases and we import them.
--
-- FIXME: Explain source_ref.  We're not populating that right now anyway.
create table sources (
    id integer primary key,
    project_id integer references projects(id) not null,
    license text not null,
    version text not null,
    source_ref text not null
);
create index sources_project_id_idx on sources(project_id);

-- A build represents a single successful compilation of a single source (hence
-- the source_id reference).  It continues the theme of spreading NEVRA-style
-- information out to several tables.  In this table we store the epoch,
-- release, and architecture since those pieces are specific to a build.
--
-- Over time, there could potentially be many entries in the table with the
-- same source_id, depending on how often upstream does releases and how
-- frequently a single release is rebuilt.
--
-- A build also has only a single changelog entry, the entry corresponding to
-- this latest build.  Constructing the entire chain of changes for a given
-- project would require grabbing all rows out of this table whose associated
-- source has an associated project with the name you're looking for.  It's a
-- little complicated, but it's expected that this sort of operation will not
-- be required often.
--
-- FIXME: Explain build_config_ref and build_env_ref.  We're not populating
-- those right now anyway.
create table builds (
    id integer primary key,
    source_id integer references sources(id) not null,
    epoch integer default 0,
    release text not null,
    arch text not null,
    build_time text not null,
    changelog blob not null,
    build_config_ref text not null,
    build_env_ref text not null
);
create index builds_source_id_idx on builds(source_id);

-- Associate various types of build signatures with a single build.  A build
-- signature could take the form of the RSA or SHA1 header out of a built RPM, or
-- a variety of other formats.  Here we store both the type of the signature and
-- the signature itself, so it can be verified by other tools.  A single build
-- can have several signatures at the same time.
create table build_signatures (
    id integer primary key,
    build_id integer references build(id) not null,
    signature_type text not null,
    signature_data blob not null
);  
create index build_signatures_build_id_idx on build_signatures(build_id);

-- Provide fixed values for what type a file can be.  This prevents it from being
-- free-form text that could potentially include values we don't know how to
-- handle.
create table file_types (
    id integer primary key,
    file_type text not null
);
insert into file_types (file_type) values
    ('regular file'),
    ('directory'),
    ('socket'),
    ('symbolic link'),
    ('block device'),
    ('character device'),
    ('FIFO');

-- This is one of the largest tables in the metadata database - the one that
-- stores a row for every file that has been imported.  This table stores
-- everything required for recreating a file on disk with the right path and
-- permissions, except for the contents of the file.  This is the metadata
-- database.  The contents live in the content store.
--
-- It is possible that a single file exists in multiple compilation units
-- (packages, for instance).  Thus, the relationship between a file and what
-- contains it must be in some other table.  See build_files for more
-- information.
--
-- It is also possible (and in fact, likely) that a single file will exist in
-- multiple builds.  Consider two builds of the same source - it is likely that
-- many of the files will be identical, and that only some will change in any
-- meaningful way.  It would be nice if we could reduce duplication and only
-- store a new row for a single file when it had real changes.  Alas, a file
-- that is identical across two builds will still have a different mtime.
--
-- Thus (for now), each new build imported will result in rows for all its file
-- being created again.
create table files (
    id integer primary key,
    path text not null,
    digest text not null,
    file_type_id integer references file_types(id) not null,
    file_mode integer not null,
    file_user text not null,
    file_group text not null,
    file_size integer not null,
    mtime integer not null,
    symlink_target text
);
create index files_path_idx on files(path);

-- This table associates a single file with a single build.  It allows for a
-- file to be a part of several builds at the same time, and for a single build
-- to contain several files.
create table build_files (
    id integer primary key,
    build_id integer references build(id) not null,
    file_id integer references files(id) not null
);
create index build_files_build_id_idx on build_files(build_id);
create index build_files_file_id_idx on build_files(file_id);

-- This table is a free form key/value association.  It allows storing data that
-- doesn't make sense anywhere else, or is more free form in nature, or just
-- doesn't fit with a traditional SQL-based database layout.  This style of data
-- can exist at many levels - projects have it, as do sources, builds, and files.
-- The key/value pairs are stored in this table, and then the association with
-- some project or source is created in a specific table.  This allows sharing
-- the key/value pair among several builds, or several files, or some combination.
--
-- Primary examples of key/value data are:
--
-- * Associating produced RPMs with a single build.  Packages are a concept that
--   exist with RPM and potentially other sources of input, but not all.  Thus
--   we do not go out of our way to model them in the database.  Using a key/val
--   allows keeping track of what group of RPMs came from a given build without
--   needing extra tables that only make sense sometimes.
-- * Associating files with an RPM.  For similar reasons, we use the key/value
--   pairing to keep track of which files make up which RPM.
-- * Keeping track of rpm-provide data.
create table key_val (
    id integer primary key,
    key_value text not null,
    val_value text not null
);

-- for key/val, it's not likely that we'll have a query that is looking up
-- a key name based on the value name. Queries will either be looking for
-- values given a key, or looking for ids based on a key/value pair.
-- So instead of an index on val_value, make the second index on both key
-- and value.
create index key_val_key_value_idx on key_val(key_value);
create index key_val_val_value_idx on key_val(key_value, val_value);

-- Associate key/value data with an individual project.  It is possible for a
-- single project to have many different key/value data pieces, or none.
create table project_values (
    id integer primary key,
    project_id integer references projects(id) not null,
    key_val_id integer references key_val(id) not null
);
create index project_values_project_id_idx on project_values(project_id);
create index project_values_key_val_id_idx on project_values(key_val_id);

-- Associate key/value data with an individual source.  It is possible for a
-- single source to have many different key/value data pieces, or none.
create table source_key_values (
    id integer primary key,
    source_id integer references sources(id) not null,
    key_val_id integer references key_val(id) not null
);
create index source_key_values_source_id_idx on source_key_values(source_id);
create index source_key_values_key_val_id_idx on source_key_values(key_val_id);

-- Associate key/value data with an individual build.  It is possible for a
-- single build to have many different key/value data pieces, or none.
create table build_key_values (
    id integer primary key,
    build_id integer references builds(id) not null,
    key_val_id integer references key_val(id) not null
);
create index build_key_values_build_id_idx on build_key_values(build_id);
create index build_key_values_key_val_id_idx on build_key_values(key_val_id);

-- Associate key/value data with an individual file.  It is possible for a
-- single file to have many different key/value data pieces, or none.
create table file_key_values (
    id integer primary key,
    file_id integer references files(id) not null,
    key_val_id integer references key_val(id) not null
);
create index file_key_values_file_id_idx on file_key_values(file_id);
create index file_key_values_key_val_id_idx on file_key_values(key_val_id);

-- Groups of things. e.g., a rpm subpackage, a comps group, a module
-- This differs from file tags in that a group can contain other groups in
-- addition to individual files, and a group can be empty.
create table groups (
    id integer primary key,
    name text not null,
    group_type text not null,
    constraint unique_name_type unique (name, group_type)
);
create index groups_name_idx on groups(name);

create table group_files (
    id integer primary key,
    group_id integer references groups(id) not null,
    file_id integer references files(id) not null
);
create index group_files_group_id_idx on group_files(group_id);
create index group_files_file_id_idx on group_files(file_id);

-- FIXME how do you prevent cycles in this thing?
create table group_groups (
    id integer primary key,
    parent_group_id references groups(id) not null,
    child_group_id references groups(id) not null
);
create index group_groups_parent_group_id_idx on group_groups(parent_group_id);
create index group_groups_child_group_id_idx on group_groups(child_group_id);

create table group_key_values (
    id integer primary key,
    group_id integer references groups(id) not null,
    key_val_id integer references key_val(id) not null
);
create index group_key_values_group_id_idx on group_key_values(group_id);
create index group_key_values_key_val_id_idx on group_key_values(key_val_id);

create table requirements (
    id integer primary key,
    req_language text not null,
    req_context text not null,
    req_strength text not null,
    req_expr text not null
);

create table group_requirements (
    id integer primary key,
    group_id integer references groups(id) not null,
    req_id integer references requirements(id) not null
);
create index group_requirements_group_id_idx on group_requirements(group_id);
create index group_requirements_req_id_idx on group_requirements(req_id);

.quit

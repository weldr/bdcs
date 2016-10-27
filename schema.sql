create table projects (
    id integer primary key,
    name text not null unique,
    summary text not null,
    description text not null,
    homepage text not null,
    upstream_vcs text not null
);

create table sources (
    id integer primary key,
    project_id integer references projects(id) not null,
    license text not null,
    version text not null,
    source_ref text not null
);

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

create table build_signatures (
    id integer primary key,
    build_id integer references build(id) not null,
    signature_type text not null,
    signature_data blob not null
);  

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

create table files (
    id integer primary key,
    path text not null,
    digest text not null,
    file_type_id references files_types(id) not null,
    file_mode integer not null,
    file_user text not null,
    file_group text not null,
    device_major integer,
    device_minor integer,
    mtime integer not null,
    symlink_target text
);

-- Extra information about a file, including information that may be a list
-- examples: type (doc, ELF binary, devel), xattr
-- heavily used types (like tag and xattr) should be indexed
create table file_attributes (
    file_id references files(id) not null,
    attribute_type text not null,
    attribute_value text
);
create index file_attributes_file_id on file_attributes (file_id);
create index file_attributes_type on file_attributes (file_id, attribute_type, attribute_value) where attribute_type == "type";
create index file_attributes_xattr on file_attributes (file_id, attribute_type, attribute_value) where attribute_type == "xattr";

create table build_files (
    build_id integer references build(id) not null,
    file_id integer references files(id) not null
);

create table key_val (
    id integer primary key,
    key_value text not null,
    val_value text not null
);

create table package_key_values (
    package_id integer references packages(id) not null,
    key_val_id integer references key_val(id) not null
);

create table source_key_values (
    source_id integer references sources(id) not null,
    key_val_id integer references key_val(id) not null
);

create table build_key_values (
    build_id integer references builds(id) not null,
    key_val_id integer references key_val(id) not null
);

create table file_key_values (
    file_id integer references files(id) not null,
    key_val_id integer references key_val(id) not null
);


## 0.6.1

* Only support building with cabal-2.x.  This is needed for private scope
  support on executables, which is needed for packaging.
* Update URLs to RPMs used in testing.

## 0.6.0

* Use the typed-process module instead of process.
* Build all programs with -threaded.  All users of bdcs will need to do the same.
* Remove script support from the database.  This wasn't being used anyway.
* Make the upstream_vcs field in the projects table optional.

## 0.5.0

* Add logging of exceptions in directorySink, runCustomizations, runHacks,
  and runTmpfiles.  This requires adding MonadBaseControl constraints to
  all these functions and MonadError constrains to those functions that
  did not already have it.
* Require a filesystem package in the list of things to export.
* Require a dracut package in the list of things to export for ostree.
* Don't assume /etc/shadow exists in the output artifact.
* Log the call to dracut in ostreeSink.

## 0.4.0

* Add BDCS.Projects.getProjectsLike, which returns projects whose names match
  the % and _ SQL wildcards.
* Add BDCS.Projects.getProjectsTotal, which returns the number of projects in
  the database.
* Add the BDCS.Export.Types module, which is useful for specifying what form
  the export artifact will take.
* BDCS.Export.export and BDCS.Export.exportAndCustomize now require an
  ExportType parameter.
* Remove BDCS.Export.Utils.supportedOutputs.  The supportedExportTypes and
  exportTypeText functions in BDCS.Export.Types can be used to produce the
  same result.
* The bdcs command line tool's export subcommand now requires a -t argument for
  specifying the form of the export artifact.  The destination argument is now
  also given with -d, instead of just bare on the command line.

## 0.3.0

* Add BDCS.Groups.getGroupsLike, which returns groups whose names match the
  % and _ SQL wildcards.
* Add BDCS.Groups.getGroupsTotal, which returns the number of groups in the
  database.
* Add BDCS.DB.firstListResult, which returns the first value from the
  non-empty result list of an SQL query.
* The types of many export-related functions have been modified to include a
  constraint on MonadLoggerIO.
* Debug logging added to exportAndCustomize, runCustomizations, ostreeSink,
  qcow2Sink, and runHacks.

## 0.2.4

* Allow building with aeson-1.3 and unordered-containers-0.2.9.

## 0.2.3

* Add BDCS.Builds.findBuilds.
* Add BDCS.Sources.findSources.

## 0.2.2

* Add BDCS.CS.fileToObjectC, which, when used as part of a conduit, fetches
  the data for a single file from a ContentStore.
* Add BDCS.Export.Customize, which provides types and functions for modifying
  the data exported from a ContentStore.
* Add BDCS.Export.exportAndCustomize, which includes Customization data in an
  export.
* Include schema.sql in the data-files.
* Bug fixes related to FSTree and symlinks.

## 0.2.1

* Add BDCS.Export.export, which is the bulk of the "bdcs export" command
  but in function form.
* Add BDCS.Exports.Utils.supportedOutputs, which returns a list of supported
  output formats.
* Ignore Enhances, Suggests, Recommends, and Supplements weak requirements
  during dependency solving.
* Many test-related updates.
* Allow building with conduit-extra-1.2.
* Many docker-related build updates.
* Support building with cabal-2.0.

## 0.2.0

* Add a module for building virtual filesystem trees.
* Remove use of partial functions.
* baseURI now returns a Maybe.
* getDbVersion now throws an error if there's no version in the database.
* Add a BadName exception that can be thrown by mkProject.

## 0.1.1

* Add a new projects function that returns a list of all projects.
* Projects is now an instance of FromJSON and ToJSON.
* Fix running cabal commands under Docker.
* Move where the epoch appears in the output of groupIdToNevra.
* Add depcloseGroupIds for dependency solving from a list of IDs.
* Add depcloseNames for dependency solving from a list of package names.
* Rename depclose to depcloseNEVRAs to make its function clearer.

## 0.1.0

* Initial release.

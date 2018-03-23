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

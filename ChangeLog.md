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

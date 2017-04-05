module Import.State(ImportState(..))
 where

import GI.OSTree(Repo)

data ImportState = ImportState { stDB :: FilePath,
                                 stRepo :: Repo }

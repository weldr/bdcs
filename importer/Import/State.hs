module Import.State(ImportState(..))
 where

data ImportState = ImportState { stDB :: FilePath }

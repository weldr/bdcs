module BDCS.Import.State(ImportState(..))
 where

import Data.ContentStore(ContentStore)

data ImportState = ImportState { stDB :: FilePath,
                                 stRepo :: ContentStore }

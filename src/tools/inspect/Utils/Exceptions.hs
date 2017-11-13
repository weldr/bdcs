module Utils.Exceptions(InspectErrors(..))
 where

import Control.Exception(Exception)

data InspectErrors = InvalidLabelError String
                   | MissingCSError
                   | MissingDBError
 deriving(Eq, Show)

instance Exception InspectErrors

module Utils.Exceptions(LsErrors(..))
 where

import Control.Exception(Exception)

data LsErrors = InvalidLabelError String
 deriving(Eq, Show)

instance Exception LsErrors

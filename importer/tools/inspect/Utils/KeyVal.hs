module Utils.KeyVal(formatKeyValList)
 where

import           Data.List(intercalate)
import qualified Data.Text as T
import           Text.Printf(printf)

import BDCS.DB(KeyVal)
import BDCS.KeyValue(formatKeyValue)

formatKeyValList :: [KeyVal] -> String
formatKeyValList []  = ""
formatKeyValList lst = printf " [%s]" (intercalate ", " (map (T.unpack . formatKeyValue) lst))

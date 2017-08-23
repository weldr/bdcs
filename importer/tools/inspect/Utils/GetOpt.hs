module Utils.GetOpt(OptClass,
                    compilerOpts)
 where

import System.Console.GetOpt

class OptClass a

compilerOpts :: OptClass a => [OptDescr (a -> a)] -> a -> [String] -> String -> IO (a, [String])
compilerOpts options defaults argv cmdName =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaults o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where
     header = "Usage: " ++ cmdName ++ " [OPTIONS]"

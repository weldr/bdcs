module Utils.GetOpt(OptClass,
                    commandLineArgs,
                    compilerOpts)
 where

import System.Console.GetOpt

class OptClass a

commandLineArgs :: [String] -> Maybe (String, FilePath, [String])
commandLineArgs argv =
    if length argv < 2
    then Nothing
    else Just (head argv, argv !! 1, drop 2 argv)

compilerOpts :: OptClass a => [OptDescr (a -> a)] -> a -> [String] -> String -> IO (a, [String])
compilerOpts options defaults argv cmdName =
    case getOpt Permute options argv of
        (o, n, [])   -> return (foldl (flip id) defaults o, n)
        (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
 where
     header = "Usage: " ++ cmdName ++ " [OPTIONS]"

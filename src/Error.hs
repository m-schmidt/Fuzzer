module Error where

import System.Exit
import System.IO

report :: String -> IO ()
report message = hPutStrLn stderr message

exitWithInfo :: String -> IO a
exitWithInfo message = do report message; exitWith ExitSuccess

exitWithError :: String -> IO a
exitWithError message = do report message; exitWith $ ExitFailure 1

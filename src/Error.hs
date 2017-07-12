module Error where

import System.Exit
import System.IO
import System.IO.Error


-- |Report message to stderr
report :: String -> IO ()
report message = hPutStrLn stderr message

-- |Report info messages and terminate the program
exitWithInfo :: String -> IO a
exitWithInfo message = do report message; exitWith ExitSuccess

-- |Report error messages and terminate the program
exitWithError :: String -> IO a
exitWithError message = do report message; exitWith $ ExitFailure 1

-- |Wrapper around catchIOError that prints error messages and terminates the program on error
catchIO :: (FilePath -> IO a) -> FilePath -> IO a
catchIO action path = catchIOError (action path) handler
  where
    handler e
      | isPermissionError e = exitWithError $ "Error: permission to access file '" ++ path ++ "' denied."
      | otherwise           = exitWithError $ "Error: unknown error while accessing file '" ++ path ++ "'."

module Main where

import Check
import Commandline
import Control.Monad
import Error
import System.Directory
import System.Environment
import System.IO.Error


-- |Wrapper around catchIOError that prints error messages and terminates the program on error
catchIO :: (FilePath -> IO a) -> FilePath -> IO a
catchIO action path = catchIOError (action path) handler
  where
    handler e
      | isPermissionError e   = exitWithError $ "Error: permission to access file '" ++ path ++ "' denied."
      | otherwise             = exitWithError $ "Error: unknown error while accessing file '" ++ path ++ "'."


-- |Check whether file at specified path exists and is executable
checkTestScript :: FilePath -> IO ()
checkTestScript path = do
    exists <- doesPathExist path
    unless (exists) $ exitWithError $ "Error: no test scipt '" ++ path ++ "' found in present working directory."
    permissions <- getPermissions path
    unless (executable permissions) $ exitWithError $ "Error: test scipt '" ++ path ++ "' is not executable."


main :: IO ()
main = do
  opts <- getArgs >>= commandLineOptions
  catchIO checkTestScript "./test.sh"
  checkWord opts

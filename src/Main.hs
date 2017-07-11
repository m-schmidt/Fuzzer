module Main where

import Check
import Commandline
import Control.Monad
import Error
import System.Directory
import System.Environment


main :: IO ()
main = do
  opts <- getArgs >>= commandLineOptions
  catchIO checkTestScript "./test.sh"
  checkWord opts


-- |Check whether file at specified path exists and is executable
checkTestScript :: FilePath -> IO ()
checkTestScript path = do
    exists <- doesPathExist path
    unless (exists) $ exitWithError $ "Error: no test scipt '" ++ path ++ "' found in present working directory."
    permissions <- getPermissions path
    unless (executable permissions) $ exitWithError $ "Error: test scipt '" ++ path ++ "' is not executable."

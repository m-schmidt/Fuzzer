module Main where

import Check
import Commandline
import System.Environment


main :: IO ()
main = do
  opts <- getArgs >>= commandLineOptions
  checkWord opts

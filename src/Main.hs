module Main where

import Check
import Commandline
import System.Environment


main :: IO ()
main = do
  opts <- getArgs >>= parseCommandLineOptions >>= checkOptionsConsistency
  case optMode opts of
    EXPR -> checkExpressions opts
    CONV -> checkConventions opts
    LOOP -> undefined

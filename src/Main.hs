module Main where

import Check
import Commandline
import System.Environment


main :: IO ()
main = do
  opts <- getArgs >>= commandLineOptions
  case optMode opts of
    EXPR -> checkExpression opts
    CONV -> checkConventions opts

module Main where

import Check
import Commandline
import System.Environment


main :: IO ()
main = do
  opts <- getArgs >>= commandLineOptions
  case optType opts of
    UINT64 -> checkWord64 $ optCount opts
    UINT32 -> checkWord32 $ optCount opts
    UINT16 -> checkWord16 $ optCount opts
    UINT8  -> checkWord8  $ optCount opts

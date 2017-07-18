-- |Module to parse the command line
module Commandline
  ( DataType(..)
  , Options(..)
  , commandLineOptions
  )
  where

import Control.Monad
import Data.Char
import Data.List
import Error
import System.Console.GetOpt
import Text.Read


-- |Supported types for testing
data DataType
  = UINT64
  | UINT32
  | UINT16
  | UINT8
  deriving (Eq,Show,Read)


-- |Command line options
data Options = Options
  { optType     :: DataType   -- ^ Base data type for expressions during testing
  , optCount    :: Int        -- ^ Maximum number of tests
  , optSize     :: Int        -- ^ Maximum size of tests
  , optShowHelp :: Bool       -- ^ Show help and terminate program
  } deriving (Eq,Show)

-- |Default values for command line options
defaultOptions :: Options
defaultOptions  = Options
  { optType     = UINT64
  , optCount    = 100
  , optSize     = 30
  , optShowHelp = False
  }

type Result a = Either String a


-- |Option descriptions for GetOpt module
options :: [OptDescr (Options -> Result Options)]
options =
  [ Option ['h','?']
      ["help"]
      (NoArg (\opts -> Right opts { optShowHelp = True }))
      "Print this help message."
  , Option ['t']
      ["type"]
      (ReqArg convertType "TYPE")
      ("Set data type for tested expressions to `uint8', `uint16', `uint32' or `uint64'. Defaults to `" ++ defaultType ++ "'.")
  , Option ['c']
      ["count"]
      (ReqArg convertCount "NUMBER")
      ("Maximum number of tests to be done. Defaults to " ++ defaultCount ++ ".")
  , Option ['s']
      ["size"]
      (ReqArg convertSize "NUMBER")
      ("Maximum size (complexity) for each test. Defaults to " ++ defaultSize ++ ".")
  ]
  where
    defaultType  = map toLower $ show $ optType defaultOptions
    defaultCount = show $ optCount defaultOptions
    defaultSize  = show $ optSize defaultOptions


convertType :: String -> Options -> Result Options
convertType s opts =
  case readMaybe $ (map toUpper) s of
    Just t -> Right opts { optType = t }
    _      -> Left $ "illegal data type `" ++ s ++ "'"

convertCount :: String -> Options -> Result Options
convertCount s opts =
  case readMaybe s of
    Just i -> Right opts { optCount = i }
    _      -> Left $ "illegal number of tests `" ++ s ++ "'"

convertSize :: String -> Options -> Result Options
convertSize s opts =
  case readMaybe s of
    Just i -> Right opts { optSize = i }
    _      -> Left $ "illegal size for test `" ++ s ++ "'"


-- |Command line handling
commandLineOptions :: [String] -> IO Options
commandLineOptions argv =
  case getOpt Permute options argv of
    (acts, _, []) -> apply acts
    (_, _, errs)  -> exitWithError $ concat (map ("Error: " ++) $ nub errs) ++ usage

  where
    apply acts = do
      case foldM (flip ($)) defaultOptions acts of
        Left err   -> exitWithError $ "Error: " ++ err ++ "\n" ++ usage
        Right opts -> do when (optShowHelp opts) $ exitWithInfo usage
                         return opts


-- |Usage info message
usage :: String
usage = usageInfo "Synopsis: fuzzer [options]" options

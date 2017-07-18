-- |Module to parse the command line
module Commandline
  ( TestMode(..)
  , DataType(..)
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


-- |Supported modes for testing
data TestMode
  = EXPR      -- ^ Test evaluation of expressions
  | CONV      -- ^ Test calling conventions
  deriving (Eq,Show,Read)

-- |Supported types for testing
data DataType
  = UINT64
  | UINT32
  | UINT16
  | UINT8
  deriving (Eq,Show,Read)


-- |Command line options
data Options = Options
  { optMode     :: TestMode   -- ^ Mode for testing
  , optExprType :: DataType   -- ^ Base data type for expression testing
  , optCount    :: Int        -- ^ Maximum number of tests
  , optSize     :: Int        -- ^ Maximum size of tests
  , optShowHelp :: Bool       -- ^ Show help and terminate program
  } deriving (Eq,Show)

-- |Default values for command line options
defaultOptions :: Options
defaultOptions  = Options
  { optMode     = EXPR
  , optExprType = UINT64
  , optCount    = 100
  , optSize     = 30
  , optShowHelp = False
  }


-- |Option descriptions for GetOpt module
options :: [OptDescr (Options -> Either String Options)]
options =
  [ Option ['h','?']
      ["help"]
      (NoArg (\opts -> Right opts { optShowHelp = True }))
      "Print this help message."
  , Option ['m']
      ["mode"]
      (ReqArg convertMode "MODE")
      ("Select mode for testing, either `expr' or `conv'. Defaults to `" ++ defaultMode ++ "'.")
  , Option ['t']
      ["type"]
      (ReqArg convertType "TYPE")
      ("Set data type for expression tests to `uint8', `uint16', `uint32' or `uint64'. Defaults to `" ++ defaultExprType ++ "'.")
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
    defaultMode     = map toLower $ show $ optMode defaultOptions
    defaultExprType = map toLower $ show $ optExprType defaultOptions
    defaultCount    = show $ optCount defaultOptions
    defaultSize     = show $ optSize defaultOptions

    convertMode s opts = case readMaybe $ (map toUpper) s of
      Just m -> Right opts { optMode = m }
      _      -> Left $ "illegal mode `" ++ s ++ "'"

    convertType s opts = case readMaybe $ (map toUpper) s of
      Just t -> Right opts { optExprType = t }
      _      -> Left $ "illegal data type `" ++ s ++ "'"

    convertCount s opts = case readMaybe s of
      Just i -> Right opts { optCount = i }
      _      -> Left $ "illegal number of tests `" ++ s ++ "'"

    convertSize s opts = case readMaybe s of
      Just i -> Right opts { optSize = i }
      _      -> Left $ "illegal size for test `" ++ s ++ "'"


-- |Parse the commandline
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

-- |Module to parse the command line
module Commandline
  ( TestMode(..)
  , DataType(..)
  , Options(..)
  , parseCommandLineOptions
  , checkOptionsConsistency
  )
  where

import Control.Monad
import Data.Char
import Data.List
import Error
import System.Console.GetOpt
import System.IO
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
  { optMode         :: TestMode   -- ^ Mode for testing
  , optExprType     :: DataType   -- ^ Base data type for expression testing
  , optNumTests     :: Int        -- ^ Maximum number of test groups
  , optChunkSize    :: Int        -- ^ Size of a test group
  , optComplexity   :: Int        -- ^ Maximum complexity of test in a group
  , optShowHelp     :: Bool       -- ^ Show help and terminate program
  , optEnableShrink :: Bool       -- ^ Try to shrink on failing tests
  } deriving (Eq,Show)

-- |Default values for command line options
defaultOptions :: Options
defaultOptions = Options
  { optMode         = EXPR
  , optExprType     = UINT64
  , optNumTests     = 100
  , optChunkSize    = 1
  , optComplexity   = 30
  , optShowHelp     = False
  , optEnableShrink = True
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
  , Option ['n']
      ["number"]
      (ReqArg (convertInt "number of tests" (\i opts -> opts { optNumTests = i })) "NUMBER")
      ("Maximum number of calls to the external test-script. Defaults to " ++ defaultNumTests ++ ".")
  , Option ['c']
      ["complexity"]
      (ReqArg (convertInt "complexity for tests" (\i opts -> opts { optComplexity = i })) "NUMBER")
      ("Maximum complexity of each test. Defaults to " ++ defaultComplexity ++ ".")
  , Option ['g']
      ["groupsize"]
      (ReqArg (convertInt "number of tests" (\i opts -> opts { optChunkSize = i })) "NUMBER")
      ("Number of tests per call to external test-script. Defaults to " ++ defaultChunkSize ++ ".")
  , Option []
      ["noshrink"]
      (NoArg (\opts -> Right opts { optEnableShrink = False }))
      "Disable shrinking on test failure."
  ]
  where
    defaultMode       = map toLower $ show $ optMode defaultOptions
    defaultExprType   = map toLower $ show $ optExprType defaultOptions
    defaultNumTests   = show $ optNumTests defaultOptions
    defaultChunkSize  = show $ optChunkSize defaultOptions
    defaultComplexity = show $ optComplexity defaultOptions

    convertMode :: String -> Options -> Either String Options
    convertMode s opts = case readMaybe $ (map toUpper) s of
      Just m -> Right opts { optMode = m }
      _      -> Left $ "illegal mode `" ++ s ++ "'"

    convertType :: String -> Options -> Either String Options
    convertType s opts = case readMaybe $ (map toUpper) s of
      Just t -> Right opts { optExprType = t }
      _      -> Left $ "illegal data type `" ++ s ++ "'"

    convertInt :: String -> (Int -> Options -> Options) -> String -> Options -> Either String Options
    convertInt desc f s opts = case readMaybe s of
      Just i | i > 0 -> Right $ f i opts
      _              -> Left  $ "illegal " ++ desc ++ " `" ++ s ++ "'"


-- |Usage info message
usage :: String
usage = usageInfo "Synopsis: fuzzer [options]" options


-- |Parse the commandline
parseCommandLineOptions :: [String] -> IO Options
parseCommandLineOptions argv =
  case getOpt Permute options argv of
    (acts, _, []) -> evalutate acts
    (_, _, errs)  -> exitWithError $ concat (map ("Error: " ++) $ nub errs) ++ usage

  where
    evalutate acts = do
      case foldM (flip ($)) defaultOptions acts of
        Left err   -> exitWithError $ "Error: " ++ err ++ "\n" ++ usage
        Right opts -> do when (optShowHelp opts) $ exitWithInfo usage
                         return opts


-- |Consistency check for options
checkOptionsConsistency :: Options -> IO Options
checkOptionsConsistency opts = do
  when (mode == CONV && complexity > 8192) $ exitWithError "Error: complexity for mode `conv' must be <= 8192."
  when (complexity > 256) $ hPutStrLn stderr "Warning: selected complexity is very large."
  when (chunkSize > 2048) $ hPutStrLn stderr "Warning: selected group size is very large."
  return opts
  where
    mode       = optMode opts
    complexity = optComplexity opts
    chunkSize  = optChunkSize opts

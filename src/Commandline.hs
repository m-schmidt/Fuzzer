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
  { optType        :: DataType   -- ^ Base data type for expressions during testing
  , optTypeRaw     :: String     -- ^ Base data type as specified on command line
  , optCount       :: Int        -- ^ Maximum number of tests
  , optCountRaw    :: String     -- ^ Maximum number as specified on command line
  , optSize        :: Int        -- ^ Maximum size of tests
  , optSizeRaw     :: String     -- ^ Maximum size as specified on command line
  , optShowHelp    :: Bool       -- ^ Show help and terminate program
  } deriving (Eq,Show)

-- |Default values for command line options
defaultOptions :: Options
defaultOptions     = Options
  { optType        = UINT64
  , optTypeRaw     = ""
  , optCount       = 100
  , optCountRaw    = ""
  , optSize        = 30
  , optSizeRaw     = ""
  , optShowHelp    = False
  }

-- |Option descriptions for GetOpt module
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h','?']
      ["help"]
      (NoArg (\opts -> opts { optShowHelp = True }))
      "Print this help message."
  , Option ['t']
      ["type"]
      (ReqArg (\s opts -> opts { optTypeRaw = s }) "TYPE")
      ("Set data type for tested expressions to 'uint64', 'uint32', 'uint16' or 'uint8'. Defaults to '" ++ defaultType ++ "'.")
  , Option ['c']
      ["count"]
      (ReqArg (\s opts -> opts { optCountRaw = s }) "NUMBER")
      ("Maximum number of tests to be done. Defaults to " ++ defaultCount ++ ".")
  , Option ['s']
      ["size"]
      (ReqArg (\s opts -> opts { optSizeRaw = s }) "NUMBER")
      ("Maximum size/complexity for a tests. Defaults to " ++ defaultSize ++ ".")
  ]
  where
    defaultType  = map toLower $ show $ optType defaultOptions
    defaultCount = show $ optCount defaultOptions
    defaultSize  = show $ optSize defaultOptions


-- |Command line handling
commandLineOptions :: [String] -> IO Options
commandLineOptions argv =
  case getOpt Permute options argv of
    (o, _, []) -> do
      when (optShowHelp opts) help
      return opts
        >>= handleType
        >>= handleInteger "number of tests" optCountRaw (\r i-> r { optCount = i })
        >>= handleInteger "size of test" optSizeRaw (\r i-> r { optSize = i })
      where
        opts = foldl (flip id) defaultOptions o
        help = exitWithInfo (usageInfo header options)

    (_, _, errs) -> exitWithError $ concat (map ("Error: "++) $ nub errs) ++ usageInfo header options


-- |Header message for usage info
header :: String
header = "Synopsis: fuzzer [options]"


-- |Check for a valid data type specification
handleType :: Options -> IO Options
handleType opts
  | null format = return opts
  | otherwise   = case readMaybe format of
                    Just t  -> return opts { optType = t }
                    Nothing -> exitWithError $ "Error: illegal data type '" ++ optTypeRaw opts ++ "'"
  where
    format = map toUpper $ optTypeRaw opts


-- |Check for a valid integer parameter
handleInteger :: String -> (Options -> String) -> (Options -> Int -> Options) -> Options -> IO Options
handleInteger name getRaw setInt opts
  | null format = return opts
  | otherwise   = case readMaybe format of
                    Just i  -> return $ setInt opts i
                    Nothing -> exitWithError $ "Error: illegal " ++ name ++ " '" ++ format ++ "'"
  where
    format = getRaw opts

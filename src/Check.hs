module Check
  ( checkExpressions
  , checkConventions
  ) where


import Commandline
import Control.Monad
import ConventionTest
import Data.Bits
import Data.ByteString.Lazy.Char8 as L (ByteString, hPutStr)
import Data.List
import Data.Word
import Error
import ExpressionTest
import System.Directory
import System.Exit
import System.IO
import System.Process
import Test.QuickCheck


-- |Check whether file at specified path exists and is executable
checkExecutable :: FilePath -> IO ()
checkExecutable path = do
    exists <- doesPathExist path
    unless (exists) $ exitWithError $ "Error: no test scipt '" ++ path ++ "' found in present working directory."
    permissions <- getPermissions path
    unless (executable permissions) $ exitWithError $ "Error: test scipt '" ++ path ++ "' is not executable."


-- |Run external test script on generated C program(s) and return whether exit code of script was OK.
runTestScript :: String -> [L.ByteString] -> IO Bool
runTestScript script inputs = do
  checkExecutable script
  -- write c program(s) into temporary files
  tmps <- forM inputs writeTmp
  -- run test script on temporary files
  (code, _, _) <- readProcessWithExitCode script tmps ""
  -- cleanup
  forM_ tmps removeFile
  -- check exit code
  return $ code == ExitSuccess

  where
    writeTmp input = do
      dir <- getTemporaryDirectory
      (name, handle) <- openTempFile dir "test.c"
      L.hPutStr handle input
      hClose handle
      return name


-- |Evaluation of expressions over unsigned integer data types
evalWord64Correct :: [Expr Word64] -> Property
evalWord64Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord32Correct :: [Expr Word32] -> Property
evalWord32Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord16Correct :: [Expr Word16] -> Property
evalWord16Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord8Correct :: [Expr Word8] -> Property
evalWord8Correct = evalExpressionCorrect $ runTestScript "./test1.sh"


-- |Run random tests to evaluate expressions
checkExpressions :: Options -> IO ()
checkExpressions opts =
  case optExprType opts of
    UINT64 -> doCheck evalWord64Correct
    UINT32 -> doCheck evalWord32Correct
    UINT16 -> doCheck evalWord16Correct
    UINT8  -> doCheck evalWord8Correct
  where
    doCheck :: (Integral a, Bits a, ExprBase a) => ([Expr a] -> Property) -> IO ()
    doCheck p = quickCheckWith stdArgs { maxSuccess=optNumTests opts } $ forAllShrink genExprs shrinkExprs p

    genExprs :: (Integral a, Bits a, ExprBase a) => Gen [Expr a]
    genExprs = genExprList (optChunkSize opts) (optComplexity opts)

    shrinkExprs :: (Integral a, Bits a, ExprBase a) => [Expr a] -> [[Expr a]]
    shrinkExprs = if optEnableShrink opts then (transpose . map shrink) else shrinkNothing


-- |Calling convention correctly passes arguments
conventionCorrect :: Bool -> [Signature] -> Property
conventionCorrect p64 = simpleConventionCorrect p64 $ runTestScript "./test2.sh"


-- |Run random tests for calling conventions
checkConventions :: Options -> IO ()
checkConventions opts =
  quickCheckWith stdArgs { maxSuccess=optNumTests opts } $ forAllShrink genSigs shrinkSigs $ conventionCorrect (optPointer64 opts)
  where
    genSigs :: Gen [Signature]
    genSigs = genSignatureList (optChunkSize opts) (optComplexity opts)

    shrinkSigs :: [Signature] -> [[Signature]]
    shrinkSigs = if optEnableShrink opts then (transpose . map shrink) else shrinkNothing

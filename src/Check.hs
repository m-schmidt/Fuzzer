module Check
  ( checkExpressions
  , checkConventions
  ) where


import Commandline
import Control.Monad
import Data.ByteString.Lazy.Char8 as L
import Data.Word
import Error
import ExpressionTest
import System.Directory
import System.Exit
import System.IO
import System.Process
import Test.QuickCheck


-- |Check whether file at specified path exists and is executable
checkTestScript :: FilePath -> IO ()
checkTestScript path = do
    exists <- doesPathExist path
    unless (exists) $ exitWithError $ "Error: no test scipt '" ++ path ++ "' found in present working directory."
    permissions <- getPermissions path
    unless (executable permissions) $ exitWithError $ "Error: test scipt '" ++ path ++ "' is not executable."


-- |Run external test script on generated 'input' C program and return whether exit code of script was OK.
runTestScript :: String -> [L.ByteString] -> IO Bool
runTestScript script inputs = do
  checkTestScript script
  -- write c code program into temporary files
  tmps <- forM inputs writeTmp
  -- run test script on input
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
evalWord64Correct :: ExprList Word64 -> Property
evalWord64Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord32Correct :: ExprList Word32 -> Property
evalWord32Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord16Correct :: ExprList Word16 -> Property
evalWord16Correct = evalExpressionCorrect $ runTestScript "./test1.sh"

evalWord8Correct :: ExprList Word8 -> Property
evalWord8Correct = evalExpressionCorrect $ runTestScript "./test1.sh"


-- |Run random tests to evaluate expressions
checkExpressions :: Options -> IO ()
checkExpressions opts =
  case optExprType opts of
    UINT64 -> quickCheckWith args evalWord64Correct
    UINT32 -> quickCheckWith args evalWord32Correct
    UINT16 -> quickCheckWith args evalWord16Correct
    UINT8  -> quickCheckWith args evalWord8Correct
  where
    args = stdArgs { maxSuccess=optCount opts, maxSize=optSize opts }


-- |Run random tests for calling conventions
checkConventions :: Options -> IO ()
checkConventions = undefined

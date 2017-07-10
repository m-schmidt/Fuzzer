module Check
  ( checkWord64
  , checkWord32
  , checkWord16
  , checkWord8
  ) where

import Data.Word
import ExpressionTest
import System.Directory
import System.Exit
import System.IO
import System.Process
import Test.QuickCheck


-- |Run external test script on generated 'input' C program and return whether exit code of script was OK.
runTestScript :: String -> IO Bool
runTestScript input = do
  -- write input program into temporary file
  tmpDir <- getTemporaryDirectory
  (tmpName, tmpHandle) <- openTempFile tmpDir "test.c"
  hPutStr tmpHandle input
  hClose tmpHandle
  -- run test script on input
  (code, _, _) <- readProcessWithExitCode "./test.sh" [tmpName] ""
  -- cleanup
  removeFile tmpName
  -- check exit code
  return $ code == ExitSuccess


-- |Evaluation of expressions over unsigned integer data types
evalWord64Correct :: Expr Word64 -> Property
evalWord64Correct = evalExpressionCorrect runTestScript

evalWord32Correct :: Expr Word32 -> Property
evalWord32Correct = evalExpressionCorrect runTestScript

evalWord16Correct :: Expr Word16 -> Property
evalWord16Correct = evalExpressionCorrect runTestScript

evalWord8Correct :: Expr Word8 -> Property
evalWord8Correct = evalExpressionCorrect runTestScript


-- |Run up to 'n' random tests.
checkWord64 :: Int -> IO ()
checkWord64 count = quickCheckWith stdArgs { maxSuccess=count } evalWord64Correct

checkWord32 :: Int -> IO ()
checkWord32 count = quickCheckWith stdArgs { maxSuccess=count } evalWord32Correct

checkWord16 :: Int -> IO ()
checkWord16 count = quickCheckWith stdArgs { maxSuccess=count } evalWord16Correct

checkWord8 :: Int -> IO ()
checkWord8 count = quickCheckWith stdArgs { maxSuccess=count } evalWord8Correct

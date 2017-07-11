module Check (checkWord) where


import Commandline
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
evalWord64Correct :: ExprList Word64 -> Property
evalWord64Correct = evalExpressionCorrect runTestScript

evalWord32Correct :: ExprList Word32 -> Property
evalWord32Correct = evalExpressionCorrect runTestScript

evalWord16Correct :: ExprList Word16 -> Property
evalWord16Correct = evalExpressionCorrect runTestScript

evalWord8Correct :: ExprList Word8 -> Property
evalWord8Correct = evalExpressionCorrect runTestScript


-- |Run random tests according options.
checkWord :: Options -> IO ()
checkWord opts =
  case optType opts of
    UINT64 -> quickCheckWith args evalWord64Correct
    UINT32 -> quickCheckWith args evalWord32Correct
    UINT16 -> quickCheckWith args evalWord16Correct
    UINT8  -> quickCheckWith args evalWord8Correct
  where
    args = stdArgs { maxSuccess=optCount opts, maxSize=optSize opts }

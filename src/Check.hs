module Check (checkIt) where

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


-- |Run up to 'n' random tests.
checkIt :: Int -> IO ()
checkIt n = quickCheckWith stdArgs { maxSuccess=n } (evalExpressionCorrect runTestScript)

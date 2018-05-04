{-# LANGUAGE QuasiQuotes #-}

module ExpressionTest
  ( module Expression
  , evalExpressionCorrect
  ) where

import Data.Bits as Bit
import Data.ByteString.Builder
import Data.Maybe
import Data.Monoid
import Expression
import qualified Data.ByteString.Lazy.Char8 as L
import Str
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- |Proposition that evaluating expressions works
evalExpressionCorrect :: (Integral a, Bits a, ExprBase a) => ([L.ByteString] -> IO Bool) -> [Expr a] -> Property
evalExpressionCorrect runScript el = monadicIO $ do
  result <- run $ runScript . genCode $ el
  assert (result == True)


-- |Generate a C code that tests evaluation of the given expression
genCode :: (Integral a, Bits a, ExprBase a) => [Expr a] -> [L.ByteString]
genCode exprs = [toLazyByteString $  codePrefix
                                  <> testFunctions (zip [1..] exprs)
                                  <> mainPrefix
                                  <> testCalls [1..length exprs]
                                  <> mainSuffix]


-- |Program head with include statements and the exit-functions
codePrefix :: Builder
codePrefix = string8 [str|
#include <stdio.h>
#include <stdlib.h>

void exit_ok(void)
{
    exit(EXIT_SUCCESS);
}

void exit_evil(int status)
{
    exit(status);
}
|]


-- |Sequence of test functions that each check one expression
testFunctions :: (Integral a, Bits a, ExprBase a) => [(Int, Expr a)] -> Builder
testFunctions = mconcat . map func
  where
    func (n, x) = prefix <> intDec n <> begin <> decls (variables x) <> test x <> end

    prefix = string8 "\nint test"
    begin  = string8 "(void)\n{\n"
    end    = string8 ");\n}\n"
    decls  = mconcat . map decl
    test x =
      let val = fromJust $ eval x
      in   string8 "    "
        <> printType val
        <> string8 " expr = "
        <> printExpr x
        <> string8 ";\n    return (expr != "
        <> printConst val


-- |Declaration of a local variable for test functions
decl :: (Integral a, Bits a, ExprBase a) => (String, a) -> Builder
decl (name, val) = string8 "    volatile "
                <> printType val
                <> string8 " "
                <> string8 name
                <> string8 " = "
                <> printConst val
                <> string8 ";\n"


-- |Head of main function before calls to test functions
mainPrefix :: Builder
mainPrefix = string8 [str|
int main(void)
{
|]


-- |Sequence of calls to test functions
testCalls :: [Int] -> Builder
testCalls = mconcat . map call
  where
    call n = prefix <> intDec n <> mid <> intDec n <> suffix
    prefix = string8 "    if (test"
    mid    = string8 "() != 0) exit_evil("
    suffix = string8 ");\n"


-- |Footer of main function after calls to test functions
mainSuffix :: Builder
mainSuffix = string8 [str|
    exit_ok();
    return 0;
}
|]

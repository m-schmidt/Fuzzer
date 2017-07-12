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
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- |Proposition that evaluating expressions works
evalExpressionCorrect :: (Integral a, Bits a, ExprBase a) => (L.ByteString -> IO Bool) -> ExprList a -> Property
evalExpressionCorrect runTest el = monadicIO $ do
  result <- run $ runTest . genCode $ el
  assert (result == True)


-- |Generates a C program that tests evaluation of the given expression
genCode :: (Integral a, Bits a, ExprBase a) => ExprList a -> L.ByteString
genCode (ExprList xs) = toLazyByteString $  code_prefix
                                         <> test_functions (zip [1..] xs)
                                         <> main_prefix
                                         <> test_calls [1..length xs]
                                         <> main_suffix


code_prefix :: Builder
code_prefix = string8 "#include <stdio.h>\n\
                      \#include <stdlib.h>\n\n\
                      \void exit_ok(void)\n\
                      \{\n\
                      \    exit (EXIT_SUCCESS);\n\
                      \}\n\n\
                      \void exit_evil(void)\n\
                      \{\n\
                      \    exit (EXIT_FAILURE);\n\
                      \}\n\n"


main_prefix :: Builder
main_prefix = string8 "int main(void)\n{\n"


main_suffix :: Builder
main_suffix = string8 "    exit_ok();\n}\n"


test_functions :: (Integral a, Bits a, ExprBase a) => [(Int, Expr a)] -> Builder
test_functions = mconcat . map function
  where
    function (n, x) = prefix <> intDec n <> begin <> decls (variables x) <> test x <> end

    prefix = string8 "int test"
    begin  = string8 "(void)\n{\n"
    end    = string8 ");\n}\n\n"
    decls  = mconcat . map decl

    test x =
      let val = fromJust $ eval x
      in   string8 "    "
        <> printType val
        <> string8 " expr = "
        <> printExpr x
        <> string8 ";\n    return (expr != "
        <> printConst val


decl :: (Integral a, Bits a, ExprBase a) => (String, a) -> Builder
decl (name, val) = string8 "    volatile "
                <> printType val
                <> string8 " "
                <> string8 name
                <> string8 " = "
                <> printConst val
                <> string8 ";\n"


test_calls :: [Int] -> Builder
test_calls = mconcat . map call
  where
    call n = prefix <> intDec n <> suffix
    prefix = string8 "    if (test"
    suffix = string8 "() != 0) exit_evil();\n"

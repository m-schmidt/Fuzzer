module ExpressionTest
  ( module Expression
  , evalExpressionCorrect
  ) where

import Data.Bits as Bit
import Data.Maybe
import Expression
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Text.Printf


-- |Proposition that evaluating expressions works
evalExpressionCorrect :: (Integral a, Bits a, ExprBase a) => (String -> IO Bool) -> ExprList a -> Property
evalExpressionCorrect runTest el = monadicIO $ do
  result <- run $ runTest . genCode $ el
  assert (result == True)


-- |Generates a C program that tests evaluating the given expression
genCode :: (Integral a, Bits a, ExprBase a) => ExprList a -> String
genCode (ExprList xs) = concat [includes, functions, main]
  where
    includes  = "#include <stdio.h>\n\n"
    functions = unlines $ map function $ zip xs [1..]

    -- declaration for local variables
    decl :: (Integral a, Bits a, ExprBase a) => (String, a) -> String
    decl (n, v)     = printf "    volatile %s %s = %s;\n" (printType v) n (printConstant v)

    -- test function for a single expression
    function :: (Integral a, Bits a, ExprBase a) => (Expr a, Int) -> String
    function (x, n) =
      let xVal  = fromJust $ eval x
          decls = concat $ map decl $ variables x
      in
        printf "int test%d(void)\n\
               \{\n\
               \%s\
               \    %s expr = %s;\n\
               \    return (expr != %s);\n\
               \}\n"
               n
               decls
               (printType xVal)
               (show x)
               (printConstant xVal)

    -- code to call n test functions
    testcalls :: Int -> String
    testcalls n = concat $ map (printf "    if (test%d() != 0) return 1;\n") [1..n]

    -- code for the main function
    main :: String
    main = printf "\n\
                  \int main (void)\n\
                  \{\n\
                  \%s\n\
                  \    return 0;\n\
                  \}\n"
                  (testcalls $ length xs)

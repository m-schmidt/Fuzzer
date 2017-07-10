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
evalExpressionCorrect :: (Integral a, Bits a, ExprBase a) => (String -> IO Bool) -> Expr a -> Property
evalExpressionCorrect runTest expr = monadicIO $ do
  result <- run $ runTest . genCode $ expr
  assert (result == True)


-- |Generates a C program that tests evaluating the given expression
genCode :: (Integral a, Bits a, ExprBase a) => Expr a -> String
genCode expr = concat [includes, globals, main]
  where
    includes      = "#include <stdio.h>\n\n"
    globals       = unlines $ map global $ variables expr
    global (n, v) = printf "static volatile %s %s = %s;" (printType v) n (printConstant v)
    exprVal       = fromJust $ eval expr
    main          = printf "\n\
                           \int main (void)\n\
                           \{\n\
                           \    %s = %s;\n\
                           \    return (expr != %s);\n\
                           \}\n" (printType exprVal) (show expr) (printConstant exprVal)

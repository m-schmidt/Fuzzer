module ExpressionTest where

import Data.Bits as Bit
import Data.Maybe
import Expression
import Numeric
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- |Random expressions
newtype TExpr = TExpr Expr
  deriving Show

instance Arbitrary TExpr where
  arbitrary = TExpr <$> (sized expr)
    where
      expr :: Int -> Gen Expr
      expr 0         = oneof [e_valSmall, e_valBig, e_varSmall, e_varBig]
      expr n | n > 0 = oneof [e_unOp n, e_arithOp1 n, e_arithOp2 n, e_shiftOp n, e_bitOp n]
      expr _         = undefined

      -- small and large immediate numbers
      e_valSmall   = Value <$> elements shift_amounts
      e_valBig     = Value <$> elements (powers ++ powers_minus_one)

      -- variables with small and large values
      e_varSmall   = mkVariable <$> elements shift_amounts
      e_varBig     = mkVariable <$> elements (powers ++ powers_minus_one)
      mkVariable i = Variable ("var_x" ++ showHex i "") i

      -- unary and binary expressions
      e_unOp n     = UnExpr <$> elements [Complement, Negate] <*> (expr $ n-1)
      e_arithOp1 n = BinExpr <$> elements [Add, Sub, Mul] <*> (expr $ n `div` 2) <*> (expr $ n `div` 2)
      e_arithOp2 n = BinExpr <$> elements [Div, Mod] <*> (expr $ n `div` 2) <*> (e_divisor $ n `div` 2)
      e_shiftOp n  = BinExpr <$> elements [Shl, Shr] <*> (expr $ n `div` 2) <*> e_amount
      e_bitOp n    = BinExpr <$> elements [And, Or, Xor] <*> (expr $ n `div` 2) <*> (expr $ n `div` 2)

      -- expression that does not evaluate to zero
      e_divisor n  = suchThat (expr n) (\e -> eval e /= Just 0)
      -- shift amounts as variable or immediate
      e_amount     = oneof [e_valSmall, e_varSmall]

      -- powers of two
      powers = [ Bit.bit i | i <- [0..63] ]
      -- powers of two minus one, i.e. numbers with only trailing one bits
      powers_minus_one = [ i - 1 | i <- powers, i /= 1]
      -- sane shift amounts
      shift_amounts = [1,7..42]


-- |Proposition that evaluating expressions works
evalExpressionCorrect :: (String -> IO Bool) -> TExpr -> Property
evalExpressionCorrect runTest e = monadicIO $ do
  result <- run $ runTest . genCode $ e
  assert (result == True)


-- |Generates a C program that tests evaluating the given expression
genCode :: TExpr -> String
genCode (TExpr e) = concat [includes, globVars, code0, exprCode, code1, exprValue, code2]
  where
    includes       = "#include <stdio.h>\n"
    globVars       = unlines $ map globVar $ variables e
    globVar (n, v) = concat ["static volatile unsigned long long ", n, " = 0x", showHex v "", ";"]
    exprCode       = show e
    exprValue      = "0x" ++ showHex (fromJust $ eval e) "ULL"

    code0 = "int main (void)\n\
            \{\n\
            \    unsigned long long expr = "
    code1 = ";\n\
            \    printf (\"expr == 0x%llx\\n\", expr);\n\
            \    return (expr != "
    code2 = ");\n\
            \}"

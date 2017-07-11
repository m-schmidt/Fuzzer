module Expression where

import Data.Bits as Bit
import Data.List
import Data.Word
import Numeric
import Test.QuickCheck


-- |Base types for expressions
class Show a => ExprBase a where
  bitWidth :: a -> Int             -- ^ width of C data type in bits
  printType :: a -> String         -- ^ print corresponding C data type
  printConstant :: a -> String     -- ^ print value as constant in C syntax

instance ExprBase Word64 where
  bitWidth _      = 64
  printType _     = "unsigned long long"
  printConstant i = "0x" ++ showHex i "ULL"

instance ExprBase Word32 where
  bitWidth _      = 32
  printType _     = "unsigned long"
  printConstant i = "0x" ++ showHex i "UL"

instance ExprBase Word16 where
  bitWidth _      = 16
  printType _     = "unsigned short"
  printConstant i = "0x" ++ showHex i "U"

instance ExprBase Word8 where
  bitWidth _      = 8
  printType _     = "unsigned char"
  printConstant i = "0x" ++ showHex i "U"


-- |Unary operations for expressions
data UnOp = Negate | Complement deriving Eq

instance Show UnOp where
  show o = case o of
    Negate     -> " - "
    Complement -> " ~ "

unop :: (Integral a, Bits a) => UnOp -> a -> a
unop o = case o of
  Negate     -> negate
  Complement -> complement


-- |Binary operations for expressions
data BinOp = Add | Sub | Mul | Div | Mod | Shl | Shr | And | Or | Xor deriving Eq

instance Show BinOp where
  show o = case o of
    Add -> " + "
    Sub -> " - "
    Mul -> " * "
    Div -> " / "
    Mod -> " % "
    Shl -> " << "
    Shr -> " >> "
    And -> " & "
    Or  -> " | "
    Xor -> " ^ "

binop :: (Integral a, Bits a) => BinOp -> a -> a -> a
binop o =
  case o of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> quot
    Mod -> rem
    Shl -> wrap shiftL
    Shr -> wrap shiftR
    And -> (Bit..&.)
    Or  -> (Bit..|.)
    Xor -> xor
  where
    wrap f x i = f x (fromIntegral i)


-- |Conditions for expressions
data Condition = Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual deriving Eq

instance Show Condition where
  show o = case o of
    Equal          -> " == "
    NotEqual       -> " != "
    LessThan       -> " < "
    GreaterThan    -> " > "
    LessOrEqual    -> " <= "
    GreaterOrEqual -> " >= "

condop :: (Integral a, Bits a) => Condition -> a -> a -> Bool
condop o = case o of
    Equal          -> (==)
    NotEqual       -> (/=)
    LessThan       -> (<)
    GreaterThan    -> (>)
    LessOrEqual    -> (<=)
    GreaterOrEqual -> (>=)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


-- |Expressions
data Expr a
  = UnExpr UnOp (Expr a)
  | BinExpr BinOp (Expr a) (Expr a)
  | CondExpr Condition (Expr a) (Expr a) (Expr a) (Expr a)
  | Value a
  | Variable String a
  deriving Eq

instance (Integral a, Bits a, ExprBase a) => Show (Expr a) where
  show = showExpr 0
    where
      showExpr :: (Integral a, Bits a, ExprBase a) => a -> Expr a -> String
      showExpr v expr = case expr of
        UnExpr o e             -> "(" ++ printType v ++ ")(" ++ show o ++ showSub e ++ ")"
        BinExpr o e1 e2        -> "(" ++ printType v ++ ")(" ++ showSub e1 ++ show o ++ showSub e2 ++ ")"
        CondExpr o e1 e2 e3 e4 -> showSub e1 ++ show o ++ showSub e2 ++ " ? " ++ showSub e3 ++ " : " ++ showSub e4
        Value i                -> printConstant i
        Variable n _           -> n

      showSub :: (Integral a, Bits a, ExprBase a) => Expr a -> String
      showSub (Value i) = printConstant i
      showSub x         = "(" ++ show x ++ ")"


-- |Evaluate an expression
eval :: (Integral a, Bits a) => Expr a -> Maybe a
eval expr = case expr of
    UnExpr o e             -> unop o <$> eval e
    BinExpr o e1 e2        -> binop o <$> eval e1 <*> (eval e2 >>= catchErr o)
    CondExpr o e1 e2 e3 e4 -> if' <$> (condop o <$> eval e1 <*> eval e2) <*> eval e3 <*> eval e4
    Value i                -> Just i
    Variable _ i           -> Just i
  where
    catchErr Div 0 = Nothing
    catchErr Mod 0 = Nothing
    catchErr _   n = Just n


-- |Extract variable name/value pairs used in an expression
variables :: Eq a => Expr a -> [(String, a)]
variables expr = nub $ case expr of
    UnExpr _ e             -> variables e
    BinExpr _ e1 e2        -> variables e1 ++ variables e2
    CondExpr _ e1 e2 e3 e4 -> variables e1 ++ variables e2 ++ variables e3 ++ variables e4
    Variable n i           -> [(n, i)]
    _                      -> []


-- |Random expressions for QuickCheck
instance (Integral a, Bits a, ExprBase a) => Arbitrary (Expr a) where
  -- |Geneator for random expressions
  arbitrary = sized $ expr 0
    where
      expr :: (Integral a, Bits a, ExprBase a) => a -> Int -> Gen (Expr a)
      expr bits n
        | n == 0    = oneof [immAmount, mkVar <$> immAmount, immAny, mkVar <$> immAny]
        | n > 0     = oneof [unaryExpr, arithExpr1, arithExpr2, shiftExpr, bitExpr, condExpr]
        | otherwise = undefined
        where
          -- immediate constants/variable from range suitable as shift amount
          immAmount = Value <$> elements amounts
          amounts   = [ fromIntegral sh | sh <- [1,4..63], sh < bitWidth bits]

          -- immediate constants/variable from full range of data type
          immAny    = Value <$> elements (powers ++ powers_m1)
          powers    = [ Bit.bit i | i <- [0..63], i < bitWidth bits]
          powers_m1 = [ i - 1 | i <- powers, i /= 1]

          -- convert immediate values into variables
          mkVar (Value i) = Variable ("var_x" ++ showHex i "") i
          mkVar _         = undefined

          -- generators for all expression forms
          unaryExpr = UnExpr
            <$> elements [Complement, Negate]
            <*> (expr bits $ n-1)

          arithExpr1 = BinExpr
            <$> elements [Add, Sub, Mul]
            <*> (expr bits $ n `div` 2)
            <*> (expr bits $ n `div` 2)

          arithExpr2 = BinExpr
            <$> elements [Div, Mod]
            <*> (expr bits $ n `div` 2)
            <*> suchThat (expr bits $ n `div` 2) ((/= Just 0) . eval)

          shiftExpr = BinExpr
            <$> elements [Shl, Shr]
            <*> (expr bits $ n `div` 2)
            <*> oneof [immAmount, mkVar <$> immAmount]

          bitExpr = BinExpr
            <$> elements [And, Or, Xor]
            <*> (expr bits $ n `div` 2)
            <*> (expr bits $ n `div` 2)

          condExpr = CondExpr
            <$> elements [Equal, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual]
            <*> (expr bits $ n `div` 2)
            <*> (expr bits $ n `div` 2)
            <*> (expr bits $ n-1)
            <*> (expr bits $ n-1)

  -- |Shrink an expression into smaller expressions
  shrink expr = filter ((/= Nothing) . eval) $ case expr of
    UnExpr o e             -> [e] ++ [UnExpr o e' | e' <- shrink e]
    BinExpr o e1 e2        -> [e1, e2] ++ [BinExpr o e1' e2' | (e1', e2') <- shrink (e1, e2)]
    CondExpr o e1 e2 e3 e4 -> [e1, e2, e3, e4] ++ [CondExpr o e1' e2' e3' e4' | (e1', e2', e3', e4') <- shrink (e1, e2, e3, e4)]
    _                      -> []

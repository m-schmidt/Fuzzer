module Expression where

import Data.Bits as Bit
import Data.ByteString.Builder
import Data.Monoid
import Data.Word
import Numeric
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as Set
import Test.QuickCheck


-- |Base types for expressions
class Show a => ExprBase a where
  printType :: a -> Builder    -- ^ Print corresponding C data type
  printConst :: a -> Builder   -- ^ Print value as constant in C syntax
  amounts :: a -> [a]          -- ^ Shift amounts
  immediates :: a -> [a]       -- ^ Selected immediate values

instance ExprBase Word64 where
  printType _  = string8 "unsigned long long"
  printConst i = string8 "0x" <> word64Hex i <> string8 "ULL"
  amounts _    = [ 1,6..63 ]
  immediates _ = [ Bit.bit i | i <- [0..63]] ++ [ Bit.bit i - 1 | i <- [1..63]]

instance ExprBase Word32 where
  printType _  = string8 "unsigned int"
  printConst i = string8 "0x" <> word32Hex i <> string8 "U"
  amounts _    = [ 1,4..31 ]
  immediates _ = [ Bit.bit i | i <- [0..31]] ++ [ Bit.bit i - 1 | i <- [1..31]]

instance ExprBase Word16 where
  printType _  = string8 "unsigned short"
  printConst i = string8 "0x" <> word16Hex i <> string8 "U"
  amounts _    = [ 1,4..16 ]
  immediates _ = [ Bit.bit i | i <- [0..15]] ++ [ Bit.bit i - 1 | i <- [1..15]]

instance ExprBase Word8 where
  printType _  = string8 "unsigned char"
  printConst i = string8 "0x" <> word8Hex i <> string8 "U"
  amounts _    = [ 1..7 ]
  immediates _ = [ Bit.bit i | i <- [0..7]] ++ [ Bit.bit i - 1 | i <- [1..7]]


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


-- |Show instance for messages of QuickCheck
instance (Integral a, Bits a, ExprBase a) => Show (Expr a) where
  show = L.unpack . toLazyByteString . printExpr


-- |Builder for bytestream of an expression in C syntax
printExpr :: (Integral a, ExprBase a) => Expr a -> Builder
printExpr = pe 0
  where
    pe :: (Integral a, ExprBase a) => a -> Expr a -> Builder
    pe v expr = case expr of
      UnExpr o e             -> char8 '(' <> printType v <> string8 ")(" <> string8 (show o) <> pse e <> char8 ')'
      BinExpr o e1 e2        -> char8 '(' <> printType v <> string8 ")(" <> pse e1 <> string8 (show o) <> pse e2 <> char8 ')'
      CondExpr o e1 e2 e3 e4 -> pse e1 <> string8 (show o) <> pse e2 <> string8 " ? " <> pse e3 <> string8 " : " <> pse e4
      Value i                -> printConst i
      Variable n _           -> string8 n

    pse (Value i) = printConst i
    pse x         = char8 '(' <> pe 0 x <> char8 ')'


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
variables :: (Eq a, Ord a) => Expr a -> [(String, a)]
variables = Set.toAscList . go Set.empty
  where
    go s expr = case expr of
      UnExpr _ e             -> go s e
      BinExpr _ e1 e2        -> foldl go s [e1, e2]
      CondExpr _ e1 e2 e3 e4 -> foldl go s [e1, e2, e3, e4]
      Variable n i           -> Set.insert (n, i) s
      _                      -> s


-- |Random expressions for QuickCheck
instance (Integral a, Bits a, ExprBase a) => Arbitrary (Expr a) where
  -- |Geneator for random expressions
  arbitrary = sized expr
    where
      expr :: (Integral a, Bits a, ExprBase a) => Int -> Gen (Expr a)
      expr n
        | n == 0    = oneof [immAmount, mkVar <$> immAmount, immAny, mkVar <$> immAny]
        | n > 0     = oneof [unaryExpr, arithExpr1, arithExpr2, shiftExpr, bitExpr, condExpr]
        | otherwise = undefined
        where
          -- immediate constants/variable from range suitable as shift amount
          immAmount = Value <$> elements (amounts 0)

          -- immediate constants/variable from full range of data type
          immAny = Value <$> elements (immediates 0)

          -- convert immediate values into variables
          mkVar (Value i) = Variable ("v_" ++ showHex i "") i
          mkVar _         = undefined

          -- generators for all expression forms
          unaryExpr = UnExpr
            <$> elements [Complement, Negate]
            <*> (expr $ n-1)

          arithExpr1 = BinExpr
            <$> elements [Add, Sub, Mul]
            <*> (expr $ n `div` 2)
            <*> (expr $ n `div` 2)

          arithExpr2 = BinExpr
            <$> elements [Div, Mod]
            <*> (expr $ n `div` 2)
            <*> suchThat (expr $ n `div` 2) ((/= Just 0) . eval)

          shiftExpr = BinExpr
            <$> elements [Shl, Shr]
            <*> (expr $ n `div` 2)
            <*> oneof [immAmount, mkVar <$> immAmount]

          bitExpr = BinExpr
            <$> elements [And, Or, Xor]
            <*> (expr $ n `div` 2)
            <*> (expr $ n `div` 2)

          condExpr = CondExpr
            <$> elements [Equal, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual]
            <*> (expr $ n `div` 3)
            <*> (expr $ n `div` 3)
            <*> (expr $ n-1)
            <*> (expr $ n-1)

  -- |Shrink an expression into smaller expressions
  shrink expr = filter ((/= Nothing) . eval) $ case expr of
    UnExpr o e             -> [e] ++ [UnExpr o e' | e' <- shrink e]
    BinExpr o e1 e2        -> [e1, e2] ++ [BinExpr o e1' e2' | (e1', e2') <- shrink (e1, e2)]
    CondExpr o e1 e2 e3 e4 -> [e1, e2, e3, e4] ++ [CondExpr o e1' e2' e3' e4' | (e1', e2', e3', e4') <- shrink (e1, e2, e3, e4)]
    _                      -> []


-- |Lists of expressions
data ExprList a = ExprList [Expr a] deriving (Eq, Show)


-- |Random expression lists of fixed size for QuickCheck
instance (Integral a, Bits a, ExprBase a) => Arbitrary (ExprList a) where
  arbitrary = ExprList <$> vector 42
  shrink (ExprList xs) = map (\x -> ExprList [x]) xs

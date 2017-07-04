module Expression where

import Data.Bits
import Data.List
import Data.Word
import Numeric


-- |Unary operations for Expressions
data UnOp = Negate | Complement deriving Eq

-- |Print operation in C syntax
instance Show UnOp where
  show o = case o of
    Negate     -> " -"
    Complement -> " ~"

-- |Operation semantics
unop :: UnOp -> Word64 -> Word64
unop o = case o of
  Negate     -> negate
  Complement -> complement


-- |Binary operations for Expressions
data BinOp = Add | Sub | Mul | Div | Mod | Shl | Shr | And | Or | Xor deriving Eq

-- |Print operation in C syntax
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

-- |Operation semantics
binop :: BinOp -> Word64 -> Word64 -> Word64
binop o =
  case o of
    Add -> (+)
    Sub -> (-)
    Mul -> (*)
    Div -> quot
    Mod -> rem
    Shl -> wrap shiftL
    Shr -> wrap shiftR
    And -> (.&.)
    Or  -> (.|.)
    Xor -> xor
  where
    wrap f x i = f x (fromIntegral i)


-- |Conditions
data Condition = Equal | NotEqual | LessThan | GreaterThan | LessOrEqual | GreaterOrEqual deriving Eq

-- |Print operation in C syntax
instance Show Condition where
  show o = case o of
    Equal          -> " == "
    NotEqual       -> " != "
    LessThan       -> " < "
    GreaterThan    -> " > "
    LessOrEqual    -> " <= "
    GreaterOrEqual -> " >= "

-- |Operation semantics
condop :: Condition -> Word64 -> Word64 -> Bool
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
data Expr
  = UnExpr UnOp Expr
  | BinExpr BinOp Expr Expr
  | CondExpr Condition Expr Expr Expr Expr
  | Value Word64
  | Variable String Word64
  deriving Eq

-- |Print expressions in C syntax
instance Show Expr where
  show e = case e of
      UnExpr o x             -> show o ++ showSub x
      BinExpr o e1 e2        -> showSub e1 ++ show o ++ showSub e2
      CondExpr o e1 e2 e3 e4 -> showSub e1 ++ show o ++ showSub e2 ++ " ? " ++ showSub e3 ++ " : " ++ showSub e4
      Value i                -> printConstant i
      Variable n _           -> n
    where
      showSub (Value i) = printConstant i
      showSub x         = "(" ++ show x ++ ")"
      printConstant i   = "0x" ++ showHex i "ULL"


-- |Evaluate an expression, handle error cases
eval :: Expr -> Maybe Word64
eval expr = case expr of
    UnExpr o e             -> unop o <$> eval e
    BinExpr Div e1 e2      -> binop Div <$> eval e1 <*> (eval e2 >>= catchZero)
    BinExpr Mod e1 e2      -> binop Mod <$> eval e1 <*> (eval e2 >>= catchZero)
    BinExpr o e1 e2        -> binop o <$> eval e1 <*> eval e2
    CondExpr o e1 e2 e3 e4 -> if' <$> (condop o <$> eval e1 <*> eval e2) <*> eval e3 <*> eval e4
    Value i                -> Just i
    Variable _ i           -> Just i
  where
    catchZero 0 = Nothing
    catchZero n = Just n


-- |Extract variable name/value pairs used in an expression
variables :: Expr -> [(String, Word64)]
variables expr = nub $ case expr of
    UnExpr _ e             -> variables e
    BinExpr _ e1 e2        -> variables e1 ++ variables e2
    CondExpr _ e1 e2 e3 e4 -> variables e1 ++ variables e2 ++ variables e3 ++ variables e4
    Variable n i           -> [(n, i)]
    _                      -> []

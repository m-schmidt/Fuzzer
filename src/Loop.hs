module Loop
  ( Loop(..)
  , printCounterType
  , printCondition
  , printConstant
  ) where

import Data.ByteString.Builder
import Data.Monoid


-- |Specification for a loop
data Loop = Loop LoopType         -- ^ Type of loop
                 CounterType      -- ^ Type of loop counter
                 ConditionType    -- ^ Exit condition
                 Constant         -- ^ Start value for loop counter
                 Constant         -- ^ Increment value for loop counter
                 Constant         -- ^ Comparison value for exit condition
                 Integer          -- ^ Number of iterations of the loop body
                 deriving (Eq, Show)


-- |Type of loop
data LoopType
  = Do
  | For
  | While
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |C data types for loop counter specifying signedness and bit width
data CounterType = CounterType
  { signed   :: Bool
  , bitwidth :: Integer
  }
  deriving (Eq, Show)


-- |C comparison operators for exit condition of loop
data ConditionType
  = LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | Equal
  | NotEqual
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |Specification for an immediate constant
data Constant = Constant CounterType Integer
  deriving (Eq, Show)


-- |Printer for loop counter data type
printCounterType :: CounterType -> Builder
printCounterType t = signPrefix <> baseType
  where
    signPrefix = string8 $ if signed t then "signed " else "unsigned "
    baseType   = string8 $ case bitwidth t of
                             8  -> "char"
                             16 -> "short"
                             32 -> "int"
                             64 -> "long long"
                             _  -> undefined


-- |Printer for comparison operators
printCondition :: ConditionType -> Builder
printCondition c = case c of
  LessThan     -> string8 " < "
  LessEqual    -> string8 " <= "
  GreaterThan  -> string8 " > "
  GreaterEqual -> string8 " >= "
  Equal        -> string8 " == "
  NotEqual     -> string8 " != "


-- |Printer for immediate constants
printConstant :: CounterType -> Integer -> Builder
printConstant t v = integerDec v <> signSuffix <> widthSuffix
  where
    signSuffix  = string8 $ if signed t then "" else "u"
    widthSuffix = string8 $ case bitwidth t of
                              32 -> "l"
                              64 -> "ll"
                              _  -> ""

module Convention
  ( Signature(..)
  , ArgumentType(..)
  , printArgumentType
  , argumentByteSize
  , genSignatureList
  ) where

import Data.ByteString.Builder
import Test.QuickCheck


-- |Datatype for function arguments: integers and floating points of different sizes and a pointer type
data ArgumentType
  = I8
  | I16
  | I32
  | I64
  | F32
  | F64
  | Pointer
  deriving (Eq, Enum, Bounded, Ord, Show)

-- |C syntax for data type
printArgumentType :: ArgumentType -> Builder
printArgumentType t = case t of
  I8      -> string8 "unsigned char"
  I16     -> string8 "unsigned short"
  I32     -> string8 "unsigned int"
  I64     -> string8 "unsigned long long"
  F32     -> string8 "float"
  F64     -> string8 "double"
  Pointer -> string8 "void *"

-- |Size of data types in bytes
argumentByteSize :: Bool -> ArgumentType -> Int
argumentByteSize p64 t = case t of
  I8      -> 1
  I16     -> 2
  I32     -> 4
  I64     -> 8
  F32     -> 4
  F64     -> 8
  Pointer -> if p64 then 8 else 4


-- |A function signature is represented by a list of its argument types
newtype Signature = Signature [ArgumentType] deriving (Eq, Ord, Show)


-- |Random signatures for QuickCheck
instance Arbitrary Signature where
  arbitrary = Signature <$> sized arguments
    where
      arguments :: Int -> Gen [ArgumentType]
      arguments n | n > 0     = vectorOf n $ elements [minBound..maxBound]
                  | otherwise = arguments 1

  shrink (Signature args) = Signature <$> shrinkList shrinkNothing args


-- |Generator for a list of expressions. The list has a fixed length `len' and each expression is sized up to `complexity'
genSignatureList :: Int -> Int -> Gen [Signature]
genSignatureList len complexity = vectorOf len $ resize complexity arbitrary

module Loop
  ( Loop(..)
  , LoopType(..)
  , Constant(..)
  , printCounterType
  , printCondition
  , printConstant
  , genLoopList
  ) where

import Test.QuickCheck
import Data.ByteString.Builder
import Data.Monoid


-- |Specification for a loop
data Loop = Loop LoopType         -- ^ Type of loop
                 CounterType      -- ^ Type of loop counter variable
                 ConditionType    -- ^ Exit condition
                 Constant         -- ^ Start value for loop counter
                 Constant         -- ^ Increment value for loop counter
                 Constant         -- ^ Comparison value for exit condition
                 Integer          -- ^ Number of iterations of the loop body
                 deriving (Eq, Show)


-- |Types of loops
data LoopType
  = Do
  | For
  | While
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |Information for data type of loop counter variable
data CounterType = CounterType
  { signed   :: Bool
  , bitwidth :: Integer
  , rMin     :: Integer
  , rMax     :: Integer
  }
  deriving (Eq, Show)


-- |Smart constructor that handles the representable range according the bit width
mkCounterType :: Bool -> Integer -> CounterType
mkCounterType s n
 | s == True && n == 64 = CounterType s n (- 2^(n-1) + 1) (2^(n-1) - 1) -- avoid signed minimum for 64bit types
 | s == True            = CounterType s n (- 2^(n-1))     (2^(n-1) - 1)
 | otherwise            = CounterType s n 0               (2^n - 1)


-- |Check whether a value is representable in a data type
inRange :: CounterType -> Integer -> Bool
inRange ct v = rMin ct <= v && v <= rMax ct


-- |C comparison operators for the exit condition of a loop
data ConditionType
  = LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual
  | NotEqual
  deriving (Eq, Enum, Bounded, Ord, Show)


-- |Specification of an immediate constant
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


-- |Random, but valid loop specifications for QuickCheck
instance Arbitrary Loop where
  arbitrary = sized validLoop
    where
      validLoop :: Int -> Gen Loop
      validLoop n = do
        lt        <- randomLoopType
        ct        <- randomCounterType
        bound     <- randomLoopBound n lt ct
        start     <- randomStartValue n ct
        -- select an increment such that the end value computed later will also be valid
        increment <- randomIncrement n ct `suchThat` \i -> (bound == 0 && inRange ct (start - i))
                                                        || (bound /= 0 && i /= 0 && inRange ct (start + bound * i))
        cond      <- randomCondition increment
        let end = loopCounterEndValue cond start bound increment
        cts       <- randomCastType ct start increment end
        cti       <- randomCastType ct start increment end
        cte       <- randomCastType ct start increment end
        return $ Loop lt ct cond (Constant cts start) (Constant cti increment) (Constant cte end) bound


-- |Random type of loop
randomLoopType :: Gen LoopType
randomLoopType = elements [Do, For, While]


-- |Random data type for the loop counter variable
randomCounterType :: Gen CounterType
randomCounterType = mkCounterType
                    <$> elements [True, False]
                    <*> randomBitWidth


-- |Random bit width for C integer data types
randomBitWidth :: Gen Integer
randomBitWidth = elements [8, 16, 32, 64]


-- |Random loop bound, sized and also depending on the types of the loop and the loop counter variable
randomLoopBound :: Int -> LoopType -> CounterType -> Gen Integer
randomLoopBound n lt ct = choose (bMin, bMax)
  where
      bMin = if lt == Do then 1 else 0
      bMax = min (fromIntegral n) ((rMax ct - rMin ct) `div` 4)


-- |Random start value for loop counter
randomStartValue :: Int -> CounterType -> Gen Integer
randomStartValue n ct
  | signed ct = frequency [(1, fromRange), (2, nearBegin), (2, nearEnd), (2, nearZero)]
  | otherwise = frequency [(1, fromRange), (3, nearBegin), (3, nearEnd)]
  where
    -- value near beginning of representable range
    nearBegin = choose (rMin ct, rMin ct + distance)
    -- value near end of representable range
    nearEnd   = choose (rMax ct - distance, rMax ct)
    -- value from full representable range
    fromRange = choose (rMin ct, rMax ct)
    -- value near zero
    nearZero  = choose (-distance, distance)
    -- maximum allowed distance
    distance  = min (fromIntegral n) ((rMax ct - rMin ct) `div` 2)


-- |Random increment value for loop counter
randomIncrement :: Int -> CounterType -> Gen Integer
randomIncrement n ct = frequency [ (2, sizedIncrement)
                                 , (2, negate <$> sizedIncrement)
                                 , (1, rangeIncrement)
                                 ]
  where
    -- value depending on complexity and representable range
    sizedIncrement = choose (0, min (fromIntegral n) rmax)
    -- value depending only on representable range
    rangeIncrement = choose (- rmax, rmax)
    -- maximum increment value depending on representable range
    rmax = rMax ct `div` 8


-- |Random abort condition for loop, depends on direction of increment
randomCondition :: Integer -> Gen ConditionType
randomCondition increment
  | increment  < 0 = elements [GreaterThan, GreaterEqual, NotEqual]
  | increment == 0 = elements [LessThan, GreaterThan]
  | otherwise      = elements [LessThan, LessEqual, NotEqual]


-- |Random type for casting the loop counter such that the cast has no effect on the loop bound
randomCastType :: CounterType -> Integer -> Integer -> Integer -> Gen CounterType
randomCastType ct s i e = oneof [return ct, randomCounterType `suchThat` valid]
  where
    valid ct' = (inRange ct' s) && (inRange ct' $ abs i) && (inRange ct' e)


-- |End value of loop counter that leads to abort of loop
loopCounterEndValue :: ConditionType -> Integer -> Integer -> Integer -> Integer
loopCounterEndValue cond start bound increment =
  case cond of
    LessThan     -> start +  bound    * increment
    LessEqual    -> start + (bound-1) * increment
    GreaterThan  -> start +  bound    * increment
    GreaterEqual -> start + (bound-1) * increment
    NotEqual     -> start +  bound    * increment


-- |Generator for a list of loop specifications. The list has a fixed length `len' and each loop is sized up to `complexity'
genLoopList :: Int -> Int -> Gen [Loop]
genLoopList len complexity = (vectorOf len $ resize complexity arbitrary)

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
 | s == True = CounterType s n (- 2^(n-1)) (2^(n-1) - 1)
 | otherwise = CounterType s n 0           (2^n - 1)


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


-- |A comparison operator can be inclusive or exclusive, i.e. comparison with the exact end value continues to loop or exits the loop
data AbortBehaviour
  = Inclusive
  | Exclusive
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
printConstant t v = integerVal <> signSuffix <> widthSuffix
  where
    integerVal | v == -9223372036854775808 = string8 "0x8000000000000000"
               | otherwise                 = integerDec v
    signSuffix  = string8 $ if signed t then "" else "u"
    widthSuffix = string8 $ case bitwidth t of
                              32 -> "l"
                              64 -> "ll"
                              _  -> ""


-- |Random, but valid loop specifications for QuickCheck
instance Arbitrary Loop where
  arbitrary = frequency [ (9, sized validLoop), (1, sized overflowLoop) ]
    where
      -- generator for valid loops
      validLoop :: Int -> Gen Loop
      validLoop n = do
        lt    <- randomLoopType
        ct    <- randomCounterType
        bound <- randomLoopBound n ct `suchThat` \b -> b /= 0 || lt /= Do
        generateLoop n lt ct bound

      -- generator for loops with at least one iteration
      generateLoop :: Int -> LoopType -> CounterType -> Integer -> Gen Loop
      generateLoop _ Do  _ 0 = undefined

      generateLoop n lt ct 0 = do
        -- arbitrary start and end values for loop counter
        start <- randomCounterValue n ct
        end   <- randomCounterValue n ct
        -- arbitrary increment
        increment <- randomIncrement n ct
        -- arbitrary condition that fails
        cond <- randomFailingCondition start end
        cts  <- randomCastType ct start increment end
        cti  <- randomCastType ct start increment end
        cte  <- randomCastType ct start increment end
        return $ Loop lt ct cond (Constant cts start) (Constant cti increment) (Constant cte end) 0

      generateLoop n lt ct bound = do
        -- arbitrary start value for loop counter
        start <- randomCounterValue n ct
        -- choose whether exactly the hitting the comparison value of the exit condition aborts the loop or leads to a final iteration
        inclusive <- elements [False, True]
        -- random increment value with constraints...
        increment <- randomIncrement n ct `suchThat` \i ->
                        -- ...it may be zero, but only for a do-loop with bound 1 and exclusive exit condition
                        (i == 0 && lt == Do && bound == 1 && not inclusive) ||
                        -- ...otherwise it must be non-zero and the final loop counter value must remain representable
                        (i /= 0 && inRange ct (start + bound * i))
        -- the condition depends on the loop mode and the direction of the loop counter
        cond <- randomCondition inclusive increment
        -- end value for check in loop exit condition
        let end = start + increment * (bound - if inclusive then 1 else 0)
        -- delta for end value w/o effect on loop bound
        delta <- randomDisturbance cond inclusive increment
        -- the final loop counter value must be used to compute optional type casts
        let final = start + bound * increment
        cts  <- randomCastType ct start increment final
        cti  <- randomCastType ct start increment final
        cte  <- randomCastType ct start increment final
        return $ Loop lt ct cond (Constant cts start) (Constant cti increment) (Constant cte $ end + delta) bound

      -- loop with bound 1 that wraps around the range of unsigned loop counter types
      overflowLoop :: Int -> Gen Loop
      overflowLoop n = do
        lt        <- randomLoopType
        ct        <- mkCounterType False <$> randomBitWidth
        let modulus = 2^(bitwidth ct)
        let bound = 1
        start     <- randomCounterValue n ct
        delta     <- randomIncrement n ct `suchThat` \i -> (i /= 0 && inRange ct (start + i))
        let end = start + delta
        let increment = delta - modulus * signum delta
        cts       <- randomCastType ct start increment end
        cte       <- randomCastType ct start increment end
        return $ Loop lt ct NotEqual (Constant cts start) (Constant ct increment) (Constant cte end) bound


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


-- |Random loop bound but sized and depending on the type of the loop counter variable
randomLoopBound :: Int -> CounterType -> Gen Integer
randomLoopBound n ct = choose (0, bMax)
  where
    bMax  = min (fromIntegral n) (range `div` 4)
    range = rMax ct - rMin ct


-- |Random start value for loop counter
randomCounterValue :: Int -> CounterType -> Gen Integer
randomCounterValue n ct
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


-- |Random disturbance of the end value that has no effect on the loop bound
randomDisturbance :: ConditionType -> Bool -> Integer -> Gen Integer
randomDisturbance NotEqual _ _ =
  return 0
randomDisturbance _ inclusive increment =
  if inclusive then inclusiveDisplacement else exclusiveDisplacement
  where
    inclusiveDisplacement
      | increment < 0 = choose (increment+1, 0)
      | increment > 0 = choose (0, increment-1)
      | otherwise     = return 0
    exclusiveDisplacement
      | increment < 0 = choose (0, -increment-1)
      | increment > 0 = choose (-increment+1, 0)
      | otherwise     = return 0


-- |Random abort condition for loop depending on mode on exit check and direction of increment
randomCondition :: Bool -> Integer -> Gen ConditionType
randomCondition inclusive increment = if inclusive then inclusiveConsition else exclusiveConsition
  where
    inclusiveConsition
      | increment < 0 = return GreaterEqual
      | increment > 0 = return LessEqual
      | otherwise     = undefined
    exclusiveConsition
      | increment < 0 = elements [GreaterThan, NotEqual]
      | increment > 0 = elements [LessThan, NotEqual]
      | otherwise     = elements [LessThan, GreaterThan, NotEqual]


-- |Random abort condition that fails for two given comparison values
randomFailingCondition :: Integer -> Integer ->  Gen ConditionType
randomFailingCondition a b
    | a < b     = elements [GreaterThan, GreaterEqual]
    | a > b     = elements [LessThan, LessEqual]
    | otherwise = return NotEqual


-- |Random type for casting the loop counter such that the cast has no effect on the loop bound
randomCastType :: CounterType -> Integer -> Integer -> Integer -> Gen CounterType
randomCastType ct s i e = oneof [return ct, randomCounterType `suchThat` valid]
  where
    valid ct' = (inRange ct' s) && (inRange ct' $ abs i) && (inRange ct' e)


-- |Generator for a list of loop specifications. The list has a fixed length `len' and each loop is sized up to `complexity'
genLoopList :: Int -> Int -> Gen [Loop]
genLoopList len complexity = (vectorOf len $ resize complexity arbitrary)

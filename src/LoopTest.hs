{-# LANGUAGE QuasiQuotes #-}

module LoopTest
  ( module Loop
  , simpleLoopboundCorrect
  ) where

import Data.ByteString.Builder
import Data.List (intersperse)
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L
import Loop
import Str
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- |Proposition that loops have correct bounds
simpleLoopboundCorrect :: Bool -> ([L.ByteString] -> IO Bool) -> [Loop] -> Property
simpleLoopboundCorrect fc runScript loops = monadicIO $ do
  result <- run $ runScript [toLazyByteString $ testProgram fc loops]
  assert (result == True)


-- |Build program-part containing all test functions
testProgram :: Bool -> [Loop] -> Builder
testProgram fc loops = testProgramPrefix
                    <> newlineSeparated functions
                    <> mainPrefix
                    <> newlineSeparated functionCalls
                    <> mainSuffix
  where
    functions        = mapi (testFunction fc) loops
    functionCalls    = mapi functionCall loops
    functionCall n _ = string8 "    if (test" <> intDec n <> string8 "() != 0) exit_evil(" <> intDec n <> string8 ");"

testProgramPrefix :: Builder
testProgramPrefix = string8 [str|
#include <stdlib.h>

#ifndef __COMPCERT__
# define __builtin_ais_annot(X) do {} while(0)
#endif

void exit_ok(void)
{
    exit(EXIT_SUCCESS);
}

void exit_evil(int status)
{
    exit(status);
}

volatile int count;

|]

mainPrefix :: Builder
mainPrefix = string8 [str|
int main(void)
{
|]

mainSuffix :: Builder
mainSuffix = string8 [str|
    exit_ok();
    return 0;
}
|]


-- |Generate the code for a test function 'n' with a given signature
testFunction :: Bool -> Int -> Loop -> Builder
testFunction fc n (Loop lt ct cond (Constant cts start) (Constant cti increment) (Constant cte end) bound)
  = incrRoutine <> textRoutine
  where
    -- routine to increment a separate iteration counter
    incrRoutine = string8 "int __attribute__ ((noinline)) test_incr" <> intDec n <> string8 "(int x)\n{\n"
               <> flowAnnot
               <> string8 "    return x+1;\n}\n\n"

    -- routine containing the test loop
    textRoutine = string8 "int __attribute__ ((noinline)) test" <> intDec n <> string8 "(void)\n{\n"
               <> string8 "    " <> printCounterType ct <> string8 " i;\n"
               <> string8 "    count = 0;\n\n"
               <> testBody
               <> string8 "\n    return (count != " <> printConstant ct bound <> string8 ");\n}\n"

    -- the actual test loop
    testBody = case lt of
      Do    -> setupCounter
            <> string8 "    do\n    {\n"
            <> incCounterStamement
            <> loopAnnot
            <> updateCount
            <> string8 "    }\n    while (" <> checkExit <> string8 ");\n"

      For   -> string8 "    for (i = " <> startValue <> string8 "; " <> checkExit <> string8 "; " <> incCounter increment <> string8 ")\n    {\n"
            <> loopAnnot
            <> updateCount
            <> string8 "    }\n"

      While -> setupCounter
            <> string8 "    while (" <> checkExit <> string8 ")\n    {\n"
            <> incCounterStamement
            <> loopAnnot
            <> updateCount
            <> string8 "    }\n"

    -- setup of loop counter
    setupCounter             = string8 "    i = " <> startValue <> string8 ";\n"

    -- increment of counter using positive immediates
    incCounterStamement      = string8 "        " <> incCounter increment <> string8 ";\n"

    incCounter i | i < 0     = string8 "i = " <> cast cti <> string8 "i - " <> printConstant ct (negate i)
                 | otherwise = string8 "i = " <> cast cti <> string8 "i + " <> printConstant ct i

    -- check of loop exit condition
    checkExit                = cast cte <> string8 "i" <> printCondition cond <> endValue

    -- loop bound annotation for loop body
    loopAnnot | bound > 0    = string8 "        __builtin_ais_annot(\"loop %here bound:" <> integerDec bound <> string8 ".." <> integerDec bound <> string8 ";\");\n"
              | otherwise    = string8 "        __builtin_ais_annot(\"instruction %here assert reachable: false;\");\n"

    -- call to separate increment routine
    updateCount              = string8 "        count = test_incr" <> intDec n <> string8 "(count);\n"

    -- flow annotation for separate increment routine
    flowAnnot | fc == False  = mempty
              | bound > 0    = string8 "    __builtin_ais_annot(\"flow sum: point(%here) == "<> integerDec bound <> string8 " point('main');\");\n"
              | otherwise    = string8 "    __builtin_ais_annot(\"instruction %here assert reachable: false;\");\n"

    -- typecast (empty when target type equals loop counter type)
    cast t | t /= ct         = string8 "(" <> printCounterType t <> string8 ")"
           | otherwise       = string8 ""

    -- casted immediate constants constant
    startValue               = cast cts <> printConstant ct start
    endValue                 = cast cte <> printConstant ct end


-- |Like Data.List.intercalate but by monoidal concat of Builders.
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = mconcat $ intersperse b bs

newlineSeparated :: [Builder] -> Builder
newlineSeparated = intercalate (char8 '\n')

-- |Like map but with an additional counter argument
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f list = map (\(i, a) -> f i a) $ zip [1..] list

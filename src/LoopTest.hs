{-# LANGUAGE QuasiQuotes #-}

module LoopTest
  ( module Loop
  , loopboundCorrect
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
loopboundCorrect :: ([L.ByteString] -> IO Bool) -> [Loop] -> Property
loopboundCorrect runScript loops = monadicIO $ do
  result <- run $ runScript [toLazyByteString $ testProgram loops]
  assert (result == True)


-- |Build program-part containing all test functions
testProgram :: [Loop] -> Builder
testProgram loops = testProgramPrefix
                 <> newlineSeparated functions
                 <> mainPrefix
                 <> newlineSeparated functionCalls
                 <> mainSuffix
  where
    functions        = mapi testFunction loops
    functionCalls    = mapi functionCall loops
    functionCall n _ = string8 "    if (test" <> intDec n <> string8 "() != 0) exit_evil(" <> intDec n <> string8 ");"

testProgramPrefix :: Builder
testProgramPrefix = string8 [str|
#include <stdlib.h>

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
testFunction :: Int -> Loop -> Builder
testFunction n spec@(Loop _ ct cond (Constant cts start) (Constant cti increment) (Constant cte end) bound)

  =  string8 "int __attribute__ ((noinline)) test_incr" <> intDec n <> string8 "(int x)\n"
  <> string8 "{\n"
  <> flowAnnot bound
  <> string8 "    return x+1;\n\
             \}\n\n"

  <> string8 "int __attribute__ ((noinline)) test" <> intDec n <> string8 "(void)\n"
  <> string8 "{\n"
  <> string8 "    " <> printCounterType ct <> string8 " i;\n"
  <> string8 "    count = 0;\n\n"
  <> testBody spec
  <> string8 "\n    return (count != " <> printConstant ct bound <> string8 ");\n\
             \}\n"

  where

    testBody (Loop Do _ _ _ _ _ _)
      =  setupCounter start
      <> string8 "    do\n    {\n"
      <> loopAnnot bound
      <> incCounter increment
      <> updateCount
      <> string8 "    }\n    while (" <> checkExit cond end <> string8 ");\n"

    testBody (Loop For _ _ _ _ _ _)
      =  string8 "    for (i = " <> printConstant ct start <> string8 "; " <> checkExit cond end <> string8 "; i += " <> printConstant ct increment <> string8 ")\n"
      <> string8 "    {\n"
      <> loopAnnot bound
      <> updateCount
      <> string8 "    }\n"

    testBody (Loop While _ _ _ _ _ _)
      =  setupCounter start
      <> string8 "    while (" <> checkExit cond end <> string8 ")\n    {\n"
      <> loopAnnot bound
      <> incCounter increment
      <> updateCount
      <> string8 "    }\n"

    -- Loop bound annottion for loop body
    loopAnnot b | b > 0      = string8 "        __builtin_ais_annot(\"loop %here bound:" <> integerDec b <> string8 ".." <> integerDec b <> string8 ";\");\n"
                | otherwise  = string8 "        __builtin_ais_annot(\"instruction %here assert reachable: false;\");\n"

    -- Setup of loop counter
    setupCounter v           = string8 "    i = " <> cast cts <> printConstant cts v <> string8 ";\n"

    -- Increment of counter using positive immediates
    incCounter i | i < 0     = string8 "        i = " <> cast cti <> string8 "i - " <> printConstant cti (negate i) <> string8 ";\n"
                 | otherwise = string8 "        i = " <> cast cti <> string8 "i + " <> printConstant cti i <> string8 ";\n"

    -- Check of loop exit condition
    checkExit c v            = cast cte <> string8 "i" <> printCondition c <> cast cte <> printConstant cte v

    -- Flow annotation for increment routine
    flowAnnot b | b > 0      = string8 "    __builtin_ais_annot(\"flow sum: point(%here) == "<> integerDec b <> string8 " point('main');\");\n"
                | otherwise  = string8 "    __builtin_ais_annot(\"instruction %here assert reachable: false;\");\n"

    -- Call to increment routine
    updateCount              = string8 "        count = test_incr" <> intDec n <> string8 "(count);\n"

    -- Typecast when target type is different
    cast t | t /= ct         = string8 "(" <> printCounterType t <> string8 ")"
           | otherwise       = string8 ""


-- |Like Data.List.intercalate but by monoidal concat of Builders.
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = mconcat $ intersperse b bs

newlineSeparated :: [Builder] -> Builder
newlineSeparated = intercalate (char8 '\n')

-- |Like map but with an additional counter argument
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f list = map (\(i, a) -> f i a) $ zip [1..] list

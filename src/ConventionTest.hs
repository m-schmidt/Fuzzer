{-# LANGUAGE QuasiQuotes #-}

module ConventionTest
  ( module Convention
  , conventionCorrect
  ) where

import Convention
import Commandline
import Data.ByteString.Builder
import Data.List (intersperse, mapAccumL)
import qualified Data.ByteString.Lazy.Char8 as L
import Str
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- |Proposition that calling convention correctly passes arguments
conventionCorrect :: Options -> ([L.ByteString] -> IO Bool) -> [Signature] -> Property
conventionCorrect opts runScript sigs = monadicIO $ do
  result <- run $ runScript [tst, drv]
  assert (result == True)
  where
    tst = toLazyByteString $ testProgram (optPointer64 opts) sigs
    drv = toLazyByteString $ driverProgram (optPointer64 opts) sigs


-- |Build program-part containing all test functions
testProgram :: Bool -> [Signature] -> Builder
testProgram p64 sigs = testPrefix <> newlineSeparated functions
  where
    functions  = mapi (testFunction p64) sigs

testPrefix :: Builder
testPrefix = string8 [str|
#include <string.h>

#ifdef DEBUG
# include <stdio.h>
#endif

#define BSIZE 65536
static unsigned char data_r [BSIZE];
extern unsigned char data_s [BSIZE];

#define BYTE_IN(x,n) (((unsigned long long)(x) >> ((n) << 3)) & 0xff)
#define BYTE_FN(x,n) (((unsigned char *)(&(x))) [sizeof(x) - 1 - (n)])

|]


-- |Generate the code for a test function 'n' with a given signature
testFunction :: Bool -> Int -> Signature -> Builder
testFunction p64 n s =  signature n s
                     <> string8 "\n{\n    int n = 0;\n\n"
                     <> argbytes s
                     <> testFunctionSuffix
  where
    argbytes (Signature args) = newlineSeparated $ mapi (extractBytes p64) args

testFunctionSuffix :: Builder
testFunctionSuffix = string8 [str|

#   ifdef DEBUG
    if (memcmp(data_s, data_r, n) != 0)
    {
        int i;
        for (i=0; i<n; i++)
        {
            printf("%3d: %02x %02x%s\n", i, data_s[i], data_r[i], data_s[i]!=data_r[i] ? " <---" : "");
        }
    }
#   endif

    return (memcmp(data_s, data_r, n) != 0);
}
|]


-- |Generate C signature for test function 'n'
signature :: Int -> Signature -> Builder
signature n (Signature args)
  =  string8 "int test"
  <> intDec n
  <> char8 '('
  <> commaSeparated (mapi argument args)
  <> char8 ')'
  where
     argument i t = printArgumentType t <> string8 " a" <> intDec i


-- |Generate statements that copy all bytes of argument 'n' with type 't' into the comparison buffer
extractBytes ::  Bool -> Int -> ArgumentType -> Builder
extractBytes p64 n t =
  let go byte | byte >= 0 = (string8 "    data_r[n++] = "
                          <> string8 (if t==F32 || t==F64 then "BYTE_FN(a" else "BYTE_IN(a")
                          <> intDec n
                          <> string8 ", "
                          <> intDec byte)
                          <> string8 ");" : go (byte - 1)
              | otherwise = []
  in
    newlineSeparated $ go $ (argumentByteSize p64 t) - 1


-- |Build program-part containing the driver function with calls to all test functions
driverProgram :: Bool -> [Signature] -> Builder
driverProgram p64 sigs = driverPrefix <> prototypes <> mainPrefix <> functions <> mainSuffix
  where
    prototypes = newlineSeparated $ mapi (\i s -> signature i s <> char8 ';') sigs
    functions  = newlineSeparated $ mapi (callTestFunction p64) sigs

driverPrefix :: Builder
driverPrefix = string8 [str|
#include <stdlib.h>

#ifndef __COMPCERT__
# define __builtin_ais_annot(X) do {} while(0)
#endif

#define BSIZE 65536
unsigned char data_s [BSIZE];

void exit_ok(void)
{
    __builtin_ais_annot("instruction %here assert reachable: true;");

    exit(EXIT_SUCCESS);
}

void exit_evil(int status)
{
#   ifdef ENABLE_PRINT_ERROR_STATUS
    printf("Test %d failed.\n", status);
#   endif

#   ifndef DISABLE_STATUS_MASKING
    if (status & 0xff == EXIT_SUCCESS) {
        status = EXIT_FAILURE;
    }
#   endif

#   ifndef DISABLE_ASSERT_REACHABLE_FALSE
    __builtin_ais_annot("instruction %here assert reachable: false;");
#   endif

    exit(status);
}

|]

mainPrefix :: Builder
mainPrefix = string8 [str|

int main(void)
{
    int n, m;
    unsigned char data_fp [8];
|]

mainSuffix :: Builder
mainSuffix = string8 [str|
    exit_ok();
    return 0;
}|]


-- |Generate statements to call test function 'n' with a given signature
callTestFunction :: Bool -> Int -> Signature -> Builder
callTestFunction p64 n (Signature args)
  =  string8 "    {\n        n = 0;\n"
  <> mconcat (mapi vardecl args)
  <> mconcat (mapiAccumL_ setup bytes args)
  <> string8 "        if (test" <> intDec n <> string8 "(" <> commaSeparated (mapiAccumL_ argument bytes args) <> string8 ") != 0) exit_evil(" <> intDec n <> string8 ");\n    }\n"
  where
    -- sequence of data bytes used to build the argument values
    bytes = drop (n*13) $ cycle [16..128]

    -- sequence of variable declarations for fp parameters
    vardecl i t
      | t==F32 || t==F64 =  string8 "        " <> printArgumentType t <> string8 " a" <> intDec i <> string8 ";\n"
      | otherwise        =  mempty

    -- sequence of statements to setup data for one argument
    setup i bs t =
      let (prefix, suffix) = splitAt (argumentByteSize p64 t) bs in
      (suffix, setup' i prefix t)

    setup' i bs t
      | t==F32 || t==F64 =  string8 "        m = 0;\n"
                         <> mconcat (map initFByte bs)
                         <> string8 "        a" <> intDec i <> string8 " = *((" <> printArgumentType t <> string8 "*)data_fp);\n"
      | otherwise        =  mconcat (map initIByte bs)

    initFByte b = string8 "        data_s [n++] = data_fp [m++] = 0x" <> wordHex b <> string8 ";\n"
    initIByte b = string8 "        data_s [n++] = 0x" <> wordHex b <> string8 ";\n"

    -- the actual arguments for the call to the test function
    argument i bs t =
      let (prefix, suffix) = splitAt (argumentByteSize p64 t) bs in
      (suffix, argument' i prefix t)

    argument' i bs t
      | t==F32 || t==F64 =  string8 "a" <> intDec i
      | t==Pointer       =  string8 "(void *)0x" <> wordHex (accumulate bs)
      | otherwise        =  string8 "0x" <> wordHex (accumulate bs)

    accumulate = foldl (\acc a -> acc*256 + a) 0


-- |Like map but with an additional counter argument
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f list = map (\(i, a) -> f i a) $ zip [1..] list

-- |Like mapAccumL map but with an additional counter argument, also the final accumulator value is ignored
mapiAccumL_ :: (Int -> a -> b -> (a, c)) -> a -> [b] -> [c]
mapiAccumL_ f a bs = let (_, cs) = mapAccumL (\acc (i, b) -> f i acc b) a (zip [1..] bs) in cs

-- |Like Data.List.intercalate but by monoidal concat of Builders.
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = mconcat $ intersperse b bs

newlineSeparated :: [Builder] -> Builder
newlineSeparated = intercalate (char8 '\n')

commaSeparated :: [Builder] -> Builder
commaSeparated = intercalate (string8 ", ")

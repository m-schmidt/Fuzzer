module ConventionTest
  ( module Convention
  , simpleConventionCorrect
  ) where

import Convention
import Data.ByteString.Builder
import Data.List (intersperse, mapAccumL)
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L
import Test.QuickCheck
import Test.QuickCheck.Monadic




-- |Proposition that calling convention correctly passes arguments
simpleConventionCorrect :: Bool -> ([L.ByteString] -> IO Bool) -> [Signature] -> Property
simpleConventionCorrect p64 runScript sigs = monadicIO $ do
  result <- run $ runScript [tst, drv]
  assert (result == True)
  where
    tst = toLazyByteString $ testProgram p64 sigs
    drv = toLazyByteString $ driverProgram p64 sigs


-- |Build program-part containing all test functions
testProgram :: Bool -> [Signature] -> Builder
testProgram p64 sigs = prefix <> newlineSeparated functions
  where
    prefix = string8 "#include <string.h>\n\n\
                     \#define BSIZE 65536\n\
                     \static unsigned char data_r [BSIZE];\n\
                     \extern unsigned char data_s [BSIZE];\n\n\
                     \#define BYTE_IN(x,n) (((unsigned long long)(x) >> ((n) << 3)) & 0xff)\n\
                     \#define BYTE_FN(x,n) (((unsigned char *)(&(x))) [sizeof(x) - 1 - (n)])\n\n"
    functions = mapi (testFunction p64) sigs


-- |Generate the code for a test function 'n' with a given signature
testFunction :: Bool -> Int -> Signature -> Builder
testFunction p64 n s =  signature n s
                     <> string8 "\n{\n    int n = 0;\n\n"
                     <> argbytes s
                     <> string8 "\n\n    return (memcmp(data_s, data_r, n) != 0);\n}\n"
  where
    argbytes (Signature args) = newlineSeparated $ mapi (extractBytes p64) args


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



-- |Build program-part containing the driver program with calls to all test functions
driverProgram :: Bool -> [Signature] -> Builder
driverProgram p64 sigs = codePrefix <> prototypes <> mainPrefix <> functions <> mainSuffix
  where
    prototypes = newlineSeparated $ mapi (\i s -> signature i s <> char8 ';') sigs
    functions  = newlineSeparated $ mapi (callTestFunction p64) sigs

    codePrefix = string8 "#include <stdlib.h>\n\n\
                         \#define BSIZE 65536\n\
                         \unsigned char data_s [BSIZE];\n\n\
                         \void exit_ok(void)\n\
                         \{\n\
                         \    exit(EXIT_SUCCESS);\n\
                         \}\n\n\
                         \void exit_evil(int status)\n\
                         \{\n\
                         \    exit(status);\n\
                         \}\n\n"

    mainPrefix = string8 "\n\nint main(void)\n\
                         \{\n\
                         \    int n, m;\n\
                         \    unsigned char data_fp [8];\n\n"

    mainSuffix = string8 "\n    exit_ok();\n\
                         \    return 0;\n\
                         \}"


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

    -- sequence of commands to setup data for one argument
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

-- |Like mapAccumL map but with an additional counter argument, also the final accumulator value is skipped
mapiAccumL_ :: (Int -> a -> b -> (a, c)) -> a -> [b] -> [c]
mapiAccumL_ f a bs = let (_, cs) = mapAccumL (\acc (i, b) -> f i acc b) a (zip [1..] bs) in cs

-- |Like Data.List.intercalate but by monoidal concat of Builders.
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = mconcat $ intersperse b bs

newlineSeparated :: [Builder] -> Builder
newlineSeparated = intercalate (char8 '\n')

commaSeparated :: [Builder] -> Builder
commaSeparated = intercalate (string8 ", ")

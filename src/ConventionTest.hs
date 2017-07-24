module ConventionTest
  ( module Convention
  , simpleConventionCorrect
  ) where

import Convention
import Data.ByteString.Builder
import Data.List (intersperse)
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as L
import Test.QuickCheck
import Test.QuickCheck.Monadic


-- |Proposition that calling convention correctly passes arguments
simpleConventionCorrect :: Bool -> ([L.ByteString] -> IO Bool) -> [Signature] -> Property
simpleConventionCorrect p64 runTest sigs = monadicIO $ do
  result <- run $ runTest [tst, drv]
  assert (result == True)
  where
    tst = toLazyByteString $ testProgram p64 sigs
    drv = toLazyByteString $ driverProgram p64 sigs


-- |Build program-part containing all test functions
testProgram :: Bool -> [Signature] -> Builder
testProgram p64 sigs = prefix <> intercalate (char8 '\n') functions
  where
    prefix = string8 "#include <string.h>\n\n\
                     \#define BSIZE 65536\n\
                     \static unsigned char data_r [BSIZE];\n\
                     \extern unsigned char data_s [BSIZE];\n\n\
                     \#define BYTE_N(x,n) (((unsigned long long)(x) >> ((n) << 3)) & 0xff)\n\n"
    functions = map (uncurry $ testFunction p64) $ zip [1..] sigs


-- |Generate the code for a test function 'n' with a given signature
testFunction :: Bool -> Int -> Signature -> Builder
testFunction p64 n s =  signature n s
                     <> string8 "\n{\n    int n = 0;\n\n"
                     <> argbytes s
                     <> string8 "\n\n    return (memcmp(data_s, data_r, n) != 0);\n}\n"
  where
    argbytes (Signature args) = intercalate (char8 '\n') $ map (uncurry $ extractBytes p64) $ zip [1..] args


-- |Generate C signature for test function 'n'
signature :: Int -> Signature -> Builder
signature n (Signature args)
  =  prefix
  <> char8 '('
  <> intercalate (string8 ", ") (map argument $ zip args [1..])
  <> char8 ')'
  where
     prefix          = string8 "int test" <> intDec n
     argument (t, i) = printArgumentType t <> string8 " a" <> intDec i


-- |Generate statements that copy all bytes of argument 'n' with type 't' into the comparison buffer
extractBytes ::  Bool -> Int -> ArgumentType -> Builder
extractBytes p64 n t =
  let go byte | byte >= 0 =  (string8 "    data_r[n++] = BYTE_N(a"
                          <> intDec n
                          <> string8 ", "
                          <> intDec byte)
                          <> string8 ");" : go (byte - 1)
              | otherwise = []
  in
    intercalate (char8 '\n') $ go $ argumentByteSize p64 t - 1


-- |Build program-part containing the driver program with calls to all test functions
driverProgram :: Bool -> [Signature] -> Builder
driverProgram p64 sigs = prefix <> prototypes <> mid <> functions <> suffix
  where
    prototypes = intercalate (char8 '\n') $ map (\(n,s) -> signature n s <> char8 ';') $ zip [1..] sigs
    functions  = intercalate (char8 '\n') $ map (uncurry $ callTestFunction p64) $ zip [1..] sigs

    prefix = string8 "#include <stdlib.h>\n\n\
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

    mid    = string8 "\n\nint main(void)\n\
                     \{\n\
                     \    int n;\n\n"

    suffix = string8 "\n    exit_ok();\n\
                     \    return 0;\n\
                     \}"


-- |Generate statements to call test function 'n' with a given signature
callTestFunction :: Bool -> Int -> Signature -> Builder
callTestFunction p64 n (Signature args)
  =  string8 "    n = 0;\n"
  <> setup args setupData
  <> string8 "    if (test"
  <> intDec n
  <> string8 "("
  <> intercalate (string8 ", ") (arguments args bytes)
  <> string8 ") != 0) exit_evil("
  <> intDec n
  <> string8 ");\n"
  where

    setup (t:ts) sdata =
      let (xs, restdata) = splitAt (argumentByteSize p64 t) sdata in
      mconcat xs <> setup ts restdata
    setup [] _ = mempty

    arguments (t:ts) bs =
      let (prefix, suffix) = splitAt (argumentByteSize p64 t) bs in
      string8 "0x" <> wordHex (accumulate prefix) : arguments ts suffix
    arguments [] _ = []

    bytes      = drop (n*13) $ cycle [16..128]
    setupData  = map (\b -> string8 "    data_s [n++] = 0x" <> wordHex b <> string8 ";\n") bytes
    accumulate = foldl (\acc a -> acc*256 + a) 0


-- |Like Data.List.intercalate but by monoidal concat of Builders.
intercalate :: Builder -> [Builder] -> Builder
intercalate b bs = mconcat $ intersperse b bs

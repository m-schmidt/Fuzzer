module ConventionTest
  ( module Convention
  , simpleConventionCorrect
  ) where

import Convention
import qualified Data.ByteString.Lazy.Char8 as L
import Test.QuickCheck


-- |Proposition that evaluating expressions works
simpleConventionCorrect :: ([L.ByteString] -> IO Bool) -> Signature -> Property
simpleConventionCorrect = undefined

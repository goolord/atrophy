{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Main (main) where

import Test.QuickCheck hiding (NonZero)
import Test.Tasty
import Test.Tasty.QuickCheck hiding (NonZero)
import qualified Test.Tasty.QuickCheck as QC (NonZero(..))
import Data.WideWord.Word128
import Data.WideWord.Word256
import Atrophy
import Data.Word

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests" [unitTests]

instance Arbitrary Word128 where
  arbitrary = Word128 <$> arbitrary <*> arbitrary

deriving via (QC.NonZero a) instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a)

-- wrong
naiveMultiply256By128UpperBits :: Word128 -> Word128 -> Word128 -> Word128
naiveMultiply256By128UpperBits aHi aLo b =
  let
    a' = Word256
      { word256hi = word128Hi64 aHi
      , word256m1 = word128Lo64 aHi
      , word256m0 = word128Hi64 aLo
      , word256lo = word128Lo64 aLo
      }
    b' = Word256
      { word256hi = word128Hi64 b
      , word256m1 = word128Lo64 b
      , word256m0 = 0
      , word256lo = 0
      }
    Word256 h l _ _ = a' * b'
  in Word128 h l

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "Long multiplication"
      [ -- testProperty "multiply256By128UpperBits" $ equivalentOnArbitrary3 multiply256By128UpperBits naiveMultiply256By128UpperBits
      ]
  , testGroup "Long division"
      [ testGroup "StrengthReducedW64" 
        [ testProperty "div64" $ \(a, b) -> ourDiv64 a b === theirDiv a b
        , testProperty "div32" $ \(a, b) -> ourDiv32 a b === theirDiv a b
        ]
      ]
  ]

ourDiv64 :: NonZero Word64 -> NonZero Word64 -> Word64
ourDiv64 (NonZero dividend) divi =
  let sr = new64 divi
  in div64 dividend sr

ourDiv32 :: NonZero Word32 -> NonZero Word32 -> Word32
ourDiv32 (NonZero dividend) divi =
  let sr = new StrengthReducedW32 divi
  in div' dividend sr

theirDiv :: Integral a => NonZero a -> NonZero a -> a
theirDiv (NonZero dividend) (NonZero divi) =
  dividend `div` divi

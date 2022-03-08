{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
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
      [ testGroup "StrengthReducedW64" [
          testProperty "div" $ \(a, b) -> ourDiv a b === theirDiv a b
        ]
      ]
  ]

ourDiv :: NonZero Word64 -> NonZero Word64 -> Word64
ourDiv (NonZero dividend) (NonZero divi) =
  let sr = new StrengthReducedW64 divi
  in div' dividend sr

theirDiv :: Integral a => NonZero a -> NonZero a -> a
theirDiv (NonZero dividend) (NonZero divi) =
  dividend `div` divi

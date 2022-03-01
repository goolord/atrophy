{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Data.Proxy
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck
import Data.WideWord.Word128
import Data.WideWord.Word256
import Data.Word
import Atrophy
import Atrophy.LongMultiplication (multiply256By128UpperBits)

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
      [ testProperty "multiply256By128UpperBits" $ equivalentOnArbitrary3 multiply256By128UpperBits naiveMultiply256By128UpperBits
      ]
  ]

equivalentOnGens3 ::
       (Show a, Show b, Show c, Show d, Eq d)
    => (a -> b -> c -> d)
    -> (a -> b -> c -> d)
    -> Gen (a, b, c)
    -> ((a, b, c) -> [(a, b, c)])
    -> Property
equivalentOnGens3 f g gen s =
    forAllShrink gen s $ \(a, b, c) -> f a b c === g a b c

equivalentOnArbitrary3 ::
       ( Show a
       , Arbitrary a
       , Show b
       , Arbitrary b
       , Show c
       , Arbitrary c
       , Show d
       , Eq d
       )
    => (a -> b -> c -> d)
    -> (a -> b -> c -> d)
    -> Property
equivalentOnArbitrary3 f g = equivalentOnGens3 f g arbitrary shrink

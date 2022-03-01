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
      { word256hi = 0
      , word256m1 = 0
      , word256m0 = word128Hi64 b
      , word256lo = word128Lo64 b
      }
  in fromIntegral (a' * b')

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testGroup "Long multiplication" 
      [ testProperty "multiply256By128UpperBits" $ equivalentOnArbitrary3 multiply256By128UpperBits naiveMultiply256By128UpperBits
      ]
  ]

equivalentOnGen ::
       (Show a, Show b, Eq b)
    => (a -> b)
    -> (a -> b)
    -> Gen a
    -> (a -> [a])
    -> Property
equivalentOnGen f g gen s = forAllShrink gen s $ \a -> f a === g a

-- |
--
-- prop> equivalentOnArbitrary ((* 2) . (+ 1)) ((+ 2) . (* 2) :: Int -> Int)
equivalentOnArbitrary ::
       (Show a, Arbitrary a, Show b, Eq b)
    => (a -> b)
    -> (a -> b)
    -> Property
equivalentOnArbitrary f g = equivalentOnGen f g arbitrary shrink

equivalentOnGens2 ::
       (Show a, Show b, Show c, Eq c)
    => (a -> b -> c)
    -> (a -> b -> c)
    -> Gen (a, b)
    -> ((a, b) -> [(a, b)])
    -> Property
equivalentOnGens2 f g gen s =
    forAllShrink gen s $ \(a, b) -> f a b === g a b

-- |
--
-- prop> equivalentOnArbitrary2 (+) ((+) :: Int -> Int -> Int)
equivalentOnArbitrary2 ::
       (Show a, Arbitrary a, Show b, Arbitrary b, Show c, Eq c)
    => (a -> b -> c)
    -> (a -> b -> c)
    -> Property
equivalentOnArbitrary2 f g = equivalentOnGens2 f g arbitrary shrink

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

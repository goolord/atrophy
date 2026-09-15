{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Atrophy
import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad (forM_, unless)
import Control.Monad.ST (runST)
import Data.Bits
import Data.Primitive.PrimArray
import Data.Proxy (Proxy (..))
import Data.WideWord.Word128
import Data.Word
import GHC.TypeNats (KnownNat, Nat, natVal, type (+), type (-), type (^))
import Test.QuickCheck hiding (NonZero (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 20000) $ testGroup "atrophy"
  [ testGroup "runtime divisors"
      [ widthTests @Word8 "Word8"
      , widthTests @Word16 "Word16"
      , widthTests @Word32 "Word32"
      , widthTests @Word64 "Word64"
      , widthTests @Word128 "Word128"
      , testCase "Word8 exhaustive" exhaustiveWord8
      , testCase "Word16 all divisors, edge numerators" (allDivisors @Word16)
      , testCase "Word32 edge divisors and numerators" (edgeCases @Word32)
      , testCase "Word64 edge divisors and numerators" (edgeCases @Word64)
      , testCase "Word128 edge divisors and numerators" (edgeCases @Word128)
      ]
  , testGroup "known divisors"
      [ testGroup "Word8"
          [ kd @1 @Word8, kd @2 @Word8, kd @3 @Word8, kd @5 @Word8, kd @7 @Word8, kd @10 @Word8
          , kd @64 @Word8, kd @127 @Word8, kd @128 @Word8, kd @129 @Word8, kd @191 @Word8, kd @255 @Word8
          ]
      , testGroup "Word16"
          [ kd @1 @Word16, kd @3 @Word16, kd @7 @Word16, kd @10 @Word16, kd @255 @Word16, kd @256 @Word16
          , kd @257 @Word16, kd @641 @Word16, kd @32767 @Word16, kd @32768 @Word16, kd @32769 @Word16, kd @65535 @Word16
          ]
      , testGroup "Word32"
          [ kd @1 @Word32, kd @3 @Word32, kd @7 @Word32, kd @10 @Word32, kd @641 @Word32, kd @65537 @Word32
          , kd @(2 ^ 31 - 1) @Word32, kd @(2 ^ 31) @Word32, kd @(2 ^ 31 + 1) @Word32, kd @1000000007 @Word32
          , kd @(2 ^ 32 - 1) @Word32
          ]
      , testGroup "Word64"
          [ kd @1 @Word64, kd @2 @Word64, kd @3 @Word64, kd @5 @Word64, kd @6 @Word64, kd @7 @Word64
          , kd @10 @Word64, kd @11 @Word64, kd @641 @Word64, kd @6700417 @Word64, kd @1000000007 @Word64
          , kd @(2 ^ 32 - 1) @Word64, kd @(2 ^ 32) @Word64, kd @(2 ^ 32 + 1) @Word64
          , kd @1000000000000000000 @Word64, kd @14757395258967641293 @Word64
          , kd @(2 ^ 63 - 1) @Word64, kd @(2 ^ 63) @Word64, kd @(2 ^ 63 + 1) @Word64, kd @(2 ^ 64 - 1) @Word64
          ]
      , testGroup "Word128"
          [ kd @1 @Word128, kd @3 @Word128, kd @7 @Word128, kd @10 @Word128, kd @641 @Word128
          , kd @(2 ^ 64 - 1) @Word128, kd @(2 ^ 64) @Word128, kd @(2 ^ 64 + 1) @Word128
          , kd @10000000000000000000 @Word128, kd @(10 ^ 30) @Word128
          , kd @(2 ^ 127 - 1) @Word128, kd @(2 ^ 127) @Word128, kd @(2 ^ 127 + 1) @Word128
          , kd @(2 ^ 128 - 159) @Word128, kd @(2 ^ 128 - 1) @Word128
          ]
      ]
  , testGroup "known numerators"
      [ testGroup "Word8"
          [ kn @0 @Word8, kn @1 @Word8, kn @2 @Word8, kn @3 @Word8, kn @128 @Word8, kn @200 @Word8, kn @255 @Word8 ]
      , testGroup "Word16"
          [ kn @0 @Word16, kn @1 @Word16, kn @2 @Word16, kn @1000 @Word16, kn @32768 @Word16, kn @65535 @Word16 ]
      , testGroup "Word32"
          [ kn @0 @Word32, kn @1 @Word32, kn @2 @Word32, kn @7 @Word32, kn @65536 @Word32, kn @1000000007 @Word32
          , kn @(2 ^ 31) @Word32, kn @(2 ^ 32 - 1) @Word32
          ]
      , testGroup "Word64"
          [ kn @0 @Word64, kn @1 @Word64, kn @2 @Word64, kn @3 @Word64, kn @1000 @Word64
          , kn @(2 ^ 31) @Word64, kn @(2 ^ 32 - 1) @Word64, kn @(2 ^ 32) @Word64, kn @(2 ^ 32 + 1) @Word64
          , kn @(2 ^ 63) @Word64, kn @(2 ^ 63 + 1) @Word64, kn @14757395258967641293 @Word64, kn @(2 ^ 64 - 1) @Word64
          ]
      , testGroup "Word128"
          [ kn @0 @Word128, kn @1 @Word128, kn @2 @Word128, kn @3 @Word128, kn @1000 @Word128
          , kn @(2 ^ 63) @Word128, kn @(2 ^ 64 - 1) @Word128, kn @(2 ^ 64) @Word128, kn @(2 ^ 64 + 1) @Word128
          , kn @(10 ^ 30) @Word128, kn @(2 ^ 127) @Word128, kn @(2 ^ 128 - 1) @Word128
          ]
      ]
  , testGroup "long division"
      [ testProperty "divRem2By1" $
          forAll nonZeroGen $ \d@(NonZero dv) -> forAll interesting $ \hi' -> forAll interesting $ \lo ->
            let hi = hi' `mod` dv
                (q, r) = (toInteger hi * 2 ^ (64 :: Int) + toInteger lo) `quotRem` toInteger dv
            in divRem2By1 (newDivisor2By1 d) hi lo === (fromInteger q, fromInteger r)
      , testProperty "divisor2By1" $
          forAll nonZeroGen $ \d@(NonZero dv) -> divisor2By1 (newDivisor2By1 d) === dv
      , testProperty "longDivision" $
          forAll (listOf interesting) $ \limbs -> forAll nonZeroGen $ \d@(NonZero dv) ->
            let (qs, r, qs', r') = runST $ do
                  let num = primArrayFromList limbs
                  quotient <- newPrimArray (length limbs + 1)
                  setPrimArray quotient 0 (length limbs + 1) 0xdeadbeef
                  rem1 <- longDivision (newDivisor2By1 d) num quotient
                  q1 <- unsafeFreezePrimArray quotient
                  inPlace <- thawPrimArray num 0 (length limbs)
                  rem2 <- longDivisionInPlace (newDivisor2By1 d) inPlace
                  q2 <- unsafeFreezePrimArray inPlace
                  pure (primArrayToList q1, rem1, primArrayToList q2, rem2)
                (qI, rI) = fromLimbs limbs `quotRem` toInteger dv
            in (fromLimbs (take (length limbs) qs), toInteger r, drop (length limbs) qs, fromLimbs qs', toInteger r')
                 === (qI, rI, [0xdeadbeef], qI, rI)
      ]
  , testGroup "long multiplication"
      [ testProperty "multiply256By128UpperBits" $
          forAll interesting $ \aHi -> forAll interesting $ \aLo -> forAll interesting $ \b ->
            toInteger (multiply256By128UpperBits aHi aLo b)
              === ((toInteger aHi * 2 ^ (128 :: Int) + toInteger aLo) * toInteger b) `shiftR` 256
      , testProperty "longMultiply" $
          forAll (listOf interesting) $ \as -> forAll interesting $ \b -> forAll (vectorOf (length as) interesting) $ \ps ->
            let prod = runST $ do
                  p <- thawPrimArray (primArrayFromList (ps ++ [0, 0])) 0 (length as + 2)
                  longMultiply (primArrayFromList as) b p
                  primArrayToList <$> unsafeFreezePrimArray p
            in fromLimbs prod === fromLimbs ps + fromLimbs as * toInteger b
      , testCase "longMultiply overflow is an error" $ do
          let as = primArrayFromList [maxBound, maxBound :: Word64]
          r <- tryEvaluate $ runST $ do
            p <- newPrimArray 2
            setPrimArray p 0 2 maxBound
            longMultiply as maxBound p
            primArrayToList <$> unsafeFreezePrimArray p
          assertBool "expected an error" (not r)
      ]
  ]
  where
  tryEvaluate :: [Word64] -> IO Bool
  tryEvaluate xs = either (\(ErrorCall _) -> False) (const True) <$> try (evaluate (length xs))

type Word' a = (Show a, Integral a, Bounded a, FiniteBits a, StrengthReduce a, Show (StrengthReduced a))

fromLimbs :: [Word64] -> Integer
fromLimbs = foldr (\l acc -> acc * 2 ^ (64 :: Int) + toInteger l) 0

ref :: Integral a => a -> NonZero a -> (a, a)
ref n (NonZero d) = case toInteger n `quotRem` toInteger d of
  (q, r) -> (fromInteger q, fromInteger r)

interesting :: forall a. (Integral a, Bounded a, FiniteBits a) => Gen a
interesting = frequency
  [ (4, chooseBoundedIntegral (minBound, maxBound))
  , (2, fromInteger <$> choose (0, 1024))
  , (2, do k <- choose (0, bits - 1); o <- choose (-3, 3); pure (fromInteger (bit k + o)))
  , (1, (maxBound -) . fromInteger <$> choose (0, 1024))
  , (2, do k <- choose (1, bits); fromInteger <$> choose (0, bit k - 1))
  ]
  where
  bits = finiteBitSize (0 :: a)

nonZeroGen :: (Integral a, Bounded a, FiniteBits a) => Gen (NonZero a)
nonZeroGen = NonZero <$> interesting `suchThat` (/= 0)

edgeValues :: forall a. (Integral a, Bounded a, FiniteBits a) => [a]
edgeValues =
  [0, 1, 2, 3, 5, 7, 10, maxBound, maxBound - 1, maxBound `div` 2, maxBound `div` 3]
    ++ concat [[bit k - 1, bit k, bit k + 1] | k <- [1 .. finiteBitSize (0 :: a) - 1]]

allChecks :: Word' a => a -> NonZero a -> [(String, (a, a))]
allChecks n d =
  let sr = new d
  in [ ("divRem", divRem n sr)
     , ("divRemConst", divRemConst n sr)
     , ("divRemNonZero", divRemNonZero n d)
     , ("divRemNonZeroConst", divRemNonZeroConst n d)
     , ("div'/rem'", (div' n sr, rem' n sr))
     , ("divConst/remConst", (divConst n sr, remConst n sr))
     , ("divNonZero/remNonZero", (divNonZero n d, remNonZero n d))
     , ("divNonZeroConst/remNonZeroConst", (divNonZeroConst n d, remNonZeroConst n d))
     ]

checkPair :: Word' a => a -> NonZero a -> Assertion
checkPair n d = forM_ (allChecks n d) $ \(name, got) ->
  unless (got == ref n d) $
    assertFailure (name ++ " " ++ show n ++ " " ++ show d ++ ": got " ++ show got ++ ", expected " ++ show (ref n d))

widthTests :: forall a. Word' a => String -> TestTree
widthTests name = testGroup name
  [ testProperty "all variants agree with quotRem" $
      forAll interesting $ \n -> forAll nonZeroGen $ \d ->
        conjoin [counterexample f (got === ref n d) | (f, got) <- allChecks @a n d]
  , testProperty "divisor" $ forAll nonZeroGen $ \d@(NonZero dv) -> divisor (new @a d) === dv
  ]

exhaustiveWord8 :: Assertion
exhaustiveWord8 = forM_ [minBound .. maxBound :: Word8] $ \n ->
  forM_ [1 .. maxBound] $ \d -> checkPair n (NonZero d)

allDivisors :: forall a. Word' a => Assertion
allDivisors = forM_ [1 .. maxBound :: a] $ \d -> forM_ (edgeValues ++ [d - 1, d, d + 1, d * 2, d * 3 - 1]) $ \n ->
  checkPair n (NonZero d)

edgeCases :: forall a. Word' a => Assertion
edgeCases = forM_ (filter (/= 0) (edgeValues @a)) $ \d -> forM_ (edgeValues ++ [d - 1, d + 1, d * 2, d * 3 - 1]) $ \n ->
  checkPair n (NonZero d)

kd :: forall (d :: Nat) a. (KnownDivisor d a, KnownNat d, Integral a, Bounded a, FiniteBits a, Show a) => TestTree
kd = testProperty (show dv) $
  conjoin [check n | n <- edgeValues ++ [d - 1, d, d + 1, d * 2 - 1, d * 2]] .&&. forAll interesting check
  where
  dv = natVal (Proxy @d)
  d = fromIntegral dv :: a
  check n = counterexample (show n) $ (divRemK @d n, divK @d n, remK @d n) === (ref n (NonZero d), fst (ref n (NonZero d)), snd (ref n (NonZero d)))

kn :: forall (n :: Nat) a. (KnownNumerator n a, KnownNat n, Word' a) => TestTree
kn = testProperty (show nv) $
  conjoin [check (NonZero d) | d <- filter (/= 0) (edgeValues ++ [n - 1, n, n + 1, n `div` 2, n `div` 3])] .&&. forAll nonZeroGen check
  where
  nv = natVal (Proxy @n)
  n = fromIntegral nv :: a
  check d = counterexample (show d) $
    ( divRemN @n (new d), divN @n (new d), remN @n (new d)
    , divRemNonZeroN @n d, divNonZeroN @n d, remNonZeroN @n d
    ) === (ref n d, fst (ref n d), snd (ref n d), ref n d, fst (ref n d), snd (ref n d))

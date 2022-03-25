{-# LANGUAGE
    CPP
  , TypeApplications
  , DataKinds
  , FlexibleContexts
  , DuplicateRecordFields
  , TypeFamilies
  , BangPatterns
  , NumericUnderscores
  , ScopedTypeVariables
#-}

module Atrophy.Internal where

import Data.WideWord.Word128
import Data.Bits
import Atrophy.Internal.LongDivision
import Control.Exception (assert)
import GHC.Records
import Data.Word

{-# INLINE isPowerOf2 #-}
isPowerOf2 :: (Bits a, Num a) => a -> Bool
isPowerOf2 x = (x .&. (x - 1)) == 0

{-# INLINE new64 #-}
new64 :: Word64 -> StrengthReducedW64
new64 divi =
  assert (divi > 0) $
  if isPowerOf2 divi
  then StrengthReducedW64 0 divi
  else
    let quotient = divide128MaxBy64 $ fromIntegral divi
    in StrengthReducedW64 (quotient + 1) divi

{-# INLINE divRem64 #-}
divRem64 ::
  ( HasField "divisor" strRed a
  , HasField "multiplier" strRed Word128
  , Num a
  , Integral a
  , FiniteBits a
  ) => a -> strRed -> (a, a)
divRem64 dividend divis =
  case getField @"multiplier" divis of
    0 ->
      let
        quotient = dividend `unsafeShiftR` (countTrailingZeros $ getField @"divisor" divis)
        remainder = dividend .&. (getField @"divisor" divis - 1)
      in (quotient, remainder)
    multiplier' ->
      let
        numerator128 = fromIntegral @_ @Word128 dividend
        multipliedHi = numerator128 * (upper128 multiplier')
        multipliedLo = upper128 (numerator128 * (lower128 multiplier'))

        quotient = fromIntegral (upper128 (multipliedHi + multipliedLo))
        remainder = dividend - quotient * getField @"divisor" divis
      in (quotient, remainder)

{-# INLINE divRem #-}
divRem :: forall strRed a b.
  ( HasField "divisor" strRed a
  , HasField "multiplier" strRed b
  , Num a
  , Integral a
  , FiniteBits a, Num b, Eq b, Integral b, FiniteBits (Half b), Bits b) => a -> strRed -> (a, a)
divRem dividend divis =
  case getField @"multiplier" divis of
    0 ->
      let
        quotient = dividend `unsafeShiftR` (countTrailingZeros $ getField @"divisor" divis)
        remainder = dividend .&. (getField @"divisor" divis - 1)
      in (quotient, remainder)
    multiplier' ->
      let
        numerator64 = fromIntegral @_ @b dividend
        multipliedHi = numerator64 * (upperHalf multiplier')
        multipliedLo = upperHalf (numerator64 * (lowerHalf multiplier'))

        quotient = fromIntegral (upperHalf (multipliedHi + multipliedLo))
        remainder = dividend - quotient * getField @"divisor" divis
      in (quotient, remainder)

new :: (Ord t, Num t, Bits t, Integral t, Bounded t, Num (Multiplier t), Bounded (Multiplier t), Integral (Multiplier t)) => ((Multiplier t) -> t -> a) -> t -> a
new con divi =
  assert (divi > 0) $
  if isPowerOf2 divi
  then con 0 divi
  else
    let quotient = maxBound `div` fromIntegral divi
    in con (quotient + 1) divi

{-# INLINE div64 #-}
div64 :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
div64 a rhs = fst $ divRem64 a rhs

{-# INLINE rem64 #-}
rem64 :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
rem64 a rhs = snd $ divRem64 a rhs

{-# INLINE div' #-}
div' ::
  ( HasField "divisor" strRed b
  , HasField "multiplier" strRed w
  , Integral b, FiniteBits b,  Integral w, FiniteBits (Half w), Bits w
  ) => b -> strRed -> b
div' a rhs = fst $ divRem a rhs

{-# INLINE rem #-}
rem ::
  ( HasField "divisor" strRed b
  , HasField "multiplier" strRed w
  , Integral b, FiniteBits b,  Integral w, FiniteBits (Half w), Bits w
  ) => b -> strRed -> b
rem a rhs = snd $ divRem a rhs

{-# INLINE lower128 #-}
lower128 :: Word128 -> Word128
lower128 (Word128 _hi low) = Word128 0 low

{-# INLINE upper128 #-}
upper128 :: Word128 -> Word128
upper128 (Word128 hi _low) = Word128 0 hi

{-# INLINE lowerHalf #-}
lowerHalf :: forall w. (Num w, Integral w) => w -> w
lowerHalf w = fromIntegral $ fromIntegral @_ @Word32 w

{-# INLINE upperHalf #-}
upperHalf :: forall w. (Integral w, Bits w, FiniteBits (Half w)) => w -> w
upperHalf w = lowerHalf $ w `unsafeShiftR` (finiteBitSize @(Half w) zeroBits)

type family Multiplier a where
  Multiplier Word64 = Word128
  Multiplier Word32 = Word64
  Multiplier Word16 = Word32
  Multiplier Word8  = Word16

type family Half a where
  Half Word128 = Word64
  Half Word64 = Word32
  Half Word32 = Word16
  Half Word16  = Word8

data StrengthReducedW64 = StrengthReducedW64 { multiplier :: {-# UNPACK #-} !Word128, divisor :: {-# UNPACK #-} !Word64 }
data StrengthReducedW32 = StrengthReducedW32 { multiplier :: {-# UNPACK #-} !Word64, divisor :: {-# UNPACK #-} !Word32 }
data StrengthReducedW16 = StrengthReducedW16 { multiplier :: {-# UNPACK #-} !Word32, divisor :: {-# UNPACK #-} !Word16 }
data StrengthReducedW8  = StrengthReducedW7  { multiplier :: {-# UNPACK #-} !Word16 , divisor :: {-# UNPACK #-} !Word8  }

data StrengthReducedW128 = StrengthReducedW128
  { multiplierHi :: {-#UNPACK #-} !Word128
  , multiplierLo :: {-#UNPACK #-} !Word128
  , divisor      :: {-#UNPACK #-} !Word128
  }

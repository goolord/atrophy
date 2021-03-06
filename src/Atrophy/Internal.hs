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
  , DerivingStrategies
  , GeneralizedNewtypeDeriving
#-}

module Atrophy.Internal where

import Data.WideWord.Word128
import Data.Bits
import Atrophy.Internal.LongDivision
import GHC.Records
import Data.Word

newtype NonZero a = NonZero a
  deriving newtype (Num, Show)

instance (Bounded a, Num a) => Bounded (NonZero a) where
  minBound = 1
  maxBound = NonZero maxBound

{-# INLINE isPowerOf2 #-}
isPowerOf2 :: (Bits a, Num a) => a -> Bool
isPowerOf2 x = (x .&. (x - 1)) == 0

{-# INLINE new64 #-}
new64 :: NonZero Word64 -> StrengthReducedW64
new64 (NonZero divi) =
  if isPowerOf2 divi
  then StrengthReducedW64 0 divi
  else
    let quotient = divide128MaxBy64 $ fromIntegral divi
    in StrengthReducedW64 (quotient + 1) divi

{-# INLINE divRem64 #-}
divRem64 ::
  ( HasField "divisor" strRed a
  , HasField "multiplier" strRed Word128
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
{-# SPECIALIZE divRem :: Word32 -> StrengthReducedW32 -> (Word32, Word32) #-}
divRem :: forall strRed a b.
  ( HasField "divisor" strRed a
  , HasField "multiplier" strRed b
  , Integral a
  , FiniteBits a, Integral b, FiniteBits (Half b), Bits b) => a -> strRed -> (a, a)
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

{-# INLINE new #-}
{-# SPECIALIZE new :: (Word64 -> Word32 -> StrengthReducedW32) -> NonZero Word32 -> StrengthReducedW32 #-}
new :: (Bits t, Integral t, Bounded (Multiplier t), Integral (Multiplier t)) =>((Multiplier t) -> t -> a) -> (NonZero t) -> a
new con (NonZero divi) =
  if isPowerOf2 divi
  then con 0 divi
  else
    let quotient = maxBound `div` fromIntegral divi
    in con (quotient + 1) divi

{-# INLINE div64 #-}
{-# SPECIALIZE div64 :: Word64 -> StrengthReducedW64 -> Word64 #-}
div64 :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
div64 a rhs = fst $ divRem64 a rhs

{-# INLINE rem64 #-}
{-# SPECIALIZE rem64 :: Word64 -> StrengthReducedW64 -> Word64 #-}
rem64 :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
rem64 a rhs = snd $ divRem64 a rhs

{-# INLINE div' #-}
{-# SPECIALIZE div' :: Word32 -> StrengthReducedW32 -> Word32 #-}
div' ::
  ( HasField "divisor" strRed b
  , HasField "multiplier" strRed w
  , Integral b, FiniteBits b,  Integral w, FiniteBits (Half w), Bits w) => b -> strRed -> b
div' a rhs = fst $ divRem a rhs

{-# INLINE rem' #-}
{-# SPECIALIZE rem' :: Word32 -> StrengthReducedW32 -> Word32 #-}
rem' ::
  ( HasField "divisor" strRed b
  , HasField "multiplier" strRed w
  , Integral b, FiniteBits b,  Integral w, FiniteBits (Half w), Bits w
  ) => b -> strRed -> b
rem' a rhs = snd $ divRem a rhs

{-# INLINE lower128 #-}
lower128 :: Word128 -> Word128
lower128 (Word128 _hi low) = Word128 0 low

{-# INLINE upper128 #-}
upper128 :: Word128 -> Word128
upper128 (Word128 hi _low) = Word128 0 hi

{-# INLINE lowerHalf #-}
lowerHalf :: forall w. ( FiniteBits (Half w), Bits w) =>w -> w
lowerHalf w = (w `unsafeShiftL` halfSize) `unsafeShiftR` halfSize
  where
  halfSize = finiteBitSize @(Half w) zeroBits

{-# INLINE upperHalf #-}
upperHalf :: forall w. ( Bits w, FiniteBits (Half w)) =>w -> w
upperHalf w = w `unsafeShiftR` halfSize
  where
  halfSize = finiteBitSize @(Half w) zeroBits

type family Multiplier a where
  Multiplier Word64 = Word128
  Multiplier Word32 = Word64
  Multiplier Word16 = Word32
  Multiplier Word8  = Word16

type family Half a where
  Half Word128 = Word64
  Half Word64 = Word32
  Half Word32 = Word16
  Half Word16 = Word8

data StrengthReducedW64 = StrengthReducedW64 { multiplier :: {-# UNPACK #-} !Word128, divisor :: {-# UNPACK #-} !Word64 }
data StrengthReducedW32 = StrengthReducedW32 { multiplier :: {-# UNPACK #-} !Word64,  divisor :: {-# UNPACK #-} !Word32 }
data StrengthReducedW16 = StrengthReducedW16 { multiplier :: {-# UNPACK #-} !Word32,  divisor :: {-# UNPACK #-} !Word16 }
data StrengthReducedW8  = StrengthReducedW7  { multiplier :: {-# UNPACK #-} !Word16,  divisor :: {-# UNPACK #-} !Word8  }

data StrengthReducedW128 = StrengthReducedW128
  { multiplierHi :: {-#UNPACK #-} !Word128
  , multiplierLo :: {-#UNPACK #-} !Word128
  , divisor      :: {-#UNPACK #-} !Word128
  }

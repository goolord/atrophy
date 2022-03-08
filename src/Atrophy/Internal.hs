{-# LANGUAGE
    CPP
  , TypeApplications
  , DataKinds
  , FlexibleContexts
  , DuplicateRecordFields
  , TypeFamilies
  , BangPatterns
  , NumericUnderscores
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

type Con t a = (Word128 -> t -> a)

{-# INLINE new #-}
new :: (Bits t, Integral t) => Con t a -> t -> a
new con divi =
  assert (divi > 0) $
  if isPowerOf2 divi
  then con 0 divi
  else
    let quotient = divide128MaxBy64 $ fromIntegral divi
    in con (quotient + 1) divi

{-# INLINE divRem #-}
divRem :: 
  ( HasField "divisor" strRed a
  , HasField "multiplier" strRed Word128
  , Num a
  , Integral a
  , FiniteBits a
  ) => a -> strRed -> (a, a)
divRem dividend divis =
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

{-# INLINE div' #-}
div' :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
div' a rhs = fst $ divRem a rhs

{-# INLINE rem' #-}
rem' :: (HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral b, FiniteBits b) =>
  b -> r -> b
rem' a rhs = snd $ divRem a rhs

{-# INLINE lower128 #-}
lower128 :: Word128 -> Word128
lower128 (Word128 _hi low) = Word128 0 low

{-# INLINE upper128 #-}
upper128 :: Word128 -> Word128
upper128 (Word128 hi _low) = Word128 0 hi

type family Multiplier a where
  Multiplier Word64 = Word128
  Multiplier Word32 = Word64
  Multiplier Word16 = Word32
  Multiplier Word8  = Word16

data StrengthReducedW64 = StrengthReducedW64 { multiplier :: {-# UNPACK #-} !(Multiplier Word64), divisor :: {-# UNPACK #-} !Word64 }
data StrengthReducedW32 = StrengthReducedW32 { multiplier :: {-# UNPACK #-} !(Multiplier Word32), divisor :: {-# UNPACK #-} !Word32 }
data StrengthReducedW16 = StrengthReducedW16 { multiplier :: {-# UNPACK #-} !(Multiplier Word16), divisor :: {-# UNPACK #-} !Word16 }

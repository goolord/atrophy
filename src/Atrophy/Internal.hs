{-# LANGUAGE 
    CPP 
  , TypeApplications
  , DataKinds
  , FlexibleContexts
  , DuplicateRecordFields
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

divRem :: (HasField "divisor" r b, HasField "multiplier" r Word128, Num b, Integral b, FiniteBits b) 
  => b -> r -> (b, b)
divRem numerator denom =
  if getField @"multiplier" denom == 0
  then (numerator `shiftR` (countTrailingZeros $ getField @"divisor" denom), numerator .&. (getField @"divisor" denom - 1))
  else 
    let
      numerator128 = fromIntegral @_ @Word128 numerator
      multipliedHi = numerator128 * (getField @"multiplier" denom `shiftR` 64)
      multipliedLo = numerator128 * (lower128 $ getField @"multiplier" denom) `shiftR` 64

      quotient = fromIntegral ((multipliedHi + multipliedLo) `shiftR` 64)
      remainder = numerator - quotient * getField @"divisor" denom
  in (quotient, remainder)

div' :: (FiniteBits b, HasField "divisor" r b, HasField "multiplier" r a1,
 Num a1, Bits a2, Eq a1, Integral a2,
 HasField "multiplier" r Word128) => a2 -> r -> a2
div' a rhs = 
  if getField @"multiplier" rhs == 0
  then
    a `shiftR` (countTrailingZeros (getField @"divisor" rhs))
  else
    let
      numerator = fromIntegral a
      multipliedHi = numerator * (getField @"multiplier" rhs `shiftR` 64)
      multipliedLo = numerator * (lower128 $ getField @"multiplier" rhs) `shiftR` 64
    in fromIntegral ((multipliedHi + multipliedLo) `shiftR` 64)

rem' :: (Num a2, HasField "divisor" r a1, HasField "multiplier" r a2,
 Eq a2, HasField "divisor" r b, HasField "multiplier" r Word128,
 Integral a1, FiniteBits b, Bits a1) =>
  a1 -> r -> a1
rem' a rhs =
  if getField @"multiplier" rhs == 0
  then a .&. (getField @"divisor" rhs - 1)
  else
    let quotient = a `div'` rhs
    in a - quotient * getField @"divisor" rhs

lower128 :: Word128 -> Word128
lower128 = fromIntegral @_ @Word128 . word128Lo64

data StrengthReducedW64 = StrengthReducedW64 { multiplier :: Word128, divisor :: Word64 }
data StrengthReducedW32 = StrengthReducedW32 { multiplier :: Word64, divisor :: Word32 }
data StrengthReducedW16 = StrengthReducedW16 { multiplier :: Word32, divisor :: Word16 }

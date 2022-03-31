{-# LANGUAGE
    TypeApplications
  , ScopedTypeVariables
  , LambdaCase
  , NumericUnderscores
#-}

module Atrophy.Internal.LongDivision where

import Data.WideWord.Word128
import Data.Word
import Data.Bits

-- divides a 128-bit number by a 64-bit divisor, returning the quotient as a 64-bit number
-- assumes that the divisor and numerator have both already been bit-shifted so that countLeadingZeros divisor == 0
{-# INLINE divide128By64Preshifted #-}
divide128By64Preshifted :: Word64 -> Word64 -> Word64 -> Word64
divide128By64Preshifted numeratorHi numeratorLo' divisor =
  let
    numeratorMid = fromIntegral @Word64 @Word128 (numeratorLo' `unsafeShiftR` 32)
    numeratorLo = fromIntegral @Word32 @Word128 (fromIntegral @Word64 @Word32 numeratorLo')
    divisorFull128 = fromIntegral @Word64 @Word128 divisor
    divisorHi = divisor `unsafeShiftR` 32

    -- To get the upper 32 bits of the quotient, we want to divide 'fullUpperNumerator' by 'divisor'
    -- but the problem is, fullUpperNumerator is a 96-bit number, meaning we would need to use u128 to do the division all at once, and the whole point of this is that we don't want to do 128 bit divison because it's slow
    -- so instead, we'll shift both the numerator and divisor right by 32, giving us a 64 bit / 32 bit division. This won't give us the exact quotient -- but it will be close.
    fullUpperNumerator = (Word128 0 numeratorHi `unsafeShiftL` 32) .|. numeratorMid
    quotientHi :: Word64
    quotientHi = min (numeratorHi `div` divisorHi) (fromIntegral $ maxBound @Word32)
    productHi = (Word128 0 quotientHi) * divisorFull128

  -- quotientHi contains our guess at what the quotient is! the problem is that we got this by ignoring the lower 32 bits of the divisor. when we account for that, the quotient might be slightly lower
  -- we will know our quotient is too high if quotient * divisor > numerator. if it is, decrement until it's in range
    (productHi', quotientHi') = clampToFull productHi quotientHi divisorFull128 fullUpperNumerator

    remainderHi = fullUpperNumerator - productHi'

  -- repeat the process using the lower half of the numerator
    fullLowerNumerator = (remainderHi `unsafeShiftL` 32) .|. numeratorLo

    quotientLo = min ((fromIntegral @_ @Word64 remainderHi) `div` divisorHi) (fromIntegral $ maxBound @Word32)
    productLo = (Word128 0 quotientLo) * divisorFull128

  -- again, quotientLo is just a guess at this point, it might be slightly too large
    (_, quotientLo') = clampToFull productLo quotientLo divisorFull128 fullLowerNumerator

  -- We now have our separate quotients, now we just have to add them together
  in (quotientHi' `unsafeShiftL` 32) .|. quotientLo'

divide128MaxBy64 :: Word64 -> Word128
divide128MaxBy64 divisor =
  let
    quotientHi = maxBound @Word64 `div` divisor;
    remainderHi = maxBound @Word64 - quotientHi * divisor;

    leadingZeros = countLeadingZeros divisor
    quotientLo = if leadingZeros >= 32
      then
        let
          numeratorMid = (remainderHi `unsafeShiftL` 32) .|. (fromIntegral (maxBound @Word32))
          quotientMid = numeratorMid `div` divisor;
          remainderMid = numeratorMid - quotientMid * divisor;

          numeratorLo = (remainderMid `unsafeShiftL` 32) .|. (fromIntegral (maxBound @Word32))
          quotientLo' = numeratorLo `div` divisor

        in (quotientMid `unsafeShiftL` 32) .|. quotientLo'
      else
        let
          numeratorHi = if leadingZeros > 0
            then (remainderHi `unsafeShiftL` leadingZeros) .|. (maxBound @Word64 `unsafeShiftR` (64 - leadingZeros))
            else remainderHi
          numeratorLo = maxBound @Word64 `unsafeShiftL` leadingZeros;
        in divide128By64Preshifted numeratorHi numeratorLo (divisor `unsafeShiftL` leadingZeros)
  in ((fromIntegral quotientHi) `unsafeShiftL` 64) .|. (fromIntegral quotientLo)

clampToFull :: Word128 -> Word64 -> Word128 -> Word128 -> (Word128, Word64)
clampToFull product' quotient' divisorFull128 fullUpperNumerator = go product' quotient'
  where
  go prod quotient =
    if prod > fullUpperNumerator
    then go (prod - divisorFull128) (quotient - 1)
    else (prod, quotient)

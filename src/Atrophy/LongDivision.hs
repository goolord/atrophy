{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE NumericUnderscores #-}

module Atrophy.LongDivision where

import Data.WideWord.Word128
import Data.Word
import qualified Data.Primitive.Contiguous as Contiguous
import Data.Primitive.Contiguous (SmallArray, Slice, Mutable, MutableSliced, Sliced)
import Control.Monad.ST.Strict (ST, runST)
import Data.STRef.Strict (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Bits
import Data.Foldable (for_)
import Data.Functor ((<&>))

type StrengthReducedU64 = ()

-- divides a 128-bit number by a 64-bit divisor, returning the quotient as a 64-bit number
-- assumes that the divisor and numerator have both already been bit-shifted so that divisor.leading_zeros() == 0
{-# INLINE divide128By64Preshifted #-}
divide128By64Preshifted :: Word64 -> Word64 -> Word64 -> Word64
divide128By64Preshifted numeratorHi numeratorLo' divisor = runST $ do
  let
    numeratorMid = fromIntegral @Word64 @Word128 (numeratorLo' `shiftR` 32)
    numeratorLo = fromIntegral @Word32 @Word128 (fromIntegral @Word64 @Word32 numeratorLo')
    divisorFull128 = fromIntegral @Word64 @Word128 divisor
    divisorHi = divisor `shiftR` 32

    -- To get the upper 32 bits of the quotient, we want to divide 'full_upper_numerator' by 'divisor'
    -- but the problem is, full_upper_numerator is a 96-bit number, meaning we would need to use u128 to do the division all at once, and the whole point of this is that we don't want to do 128 bit divison because it's slow
    -- so instead, we'll shift both the numerator and divisor right by 32, giving us a 64 bit / 32 bit division. This won't give us the exact quotient -- but it will be close.
    fullUpperNumerator = (Word128 0 numeratorHi `shiftL` 32) .|. numeratorMid
    quotientHi' = min (numeratorHi `div` divisorHi) (fromIntegral $ maxBound @Word32)
  quotientHi <- newSTRef quotientHi'
  productHi <- newSTRef $ (Word128 0 quotientHi') * divisorFull128

  -- quotient_hi contains our guess at what the quotient is! the problem is that we got this by ignoring the lower 32 bits of the divisor. when we account for that, the quotient might be slightly lower
  -- we will know our quotient is too high if quotient * divisor > numerator. if it is, decrement until it's in range
  whileM_ ((>) <$> readSTRef productHi <*> pure fullUpperNumerator) $ do
    modifySTRef quotientHi (\x -> x - 1)
    modifySTRef productHi (\x -> x - divisorFull128)

  remainderHi <- readSTRef productHi <&> ((-) fullUpperNumerator)

  -- repeat the process using the lower half of the numerator
  let fullLowerNumerator = (remainderHi `shiftL` 32) .|. numeratorLo

  quotientLo <- newSTRef $ min ((fromIntegral @_ @Word64 remainderHi) `div` divisorHi) (fromIntegral $ maxBound @Word32)
  productLo  <- do 
    x <- readSTRef quotientLo
    newSTRef $ (Word128 0 x) * divisorFull128

  -- again, quotient_lo is just a guess at this point, it might be slightly too large
  whileM_ ((>) <$> readSTRef productLo <*> pure fullLowerNumerator) $ do
    modifySTRef quotientLo (\x -> x - 1)
    modifySTRef productLo (\x -> x - divisorFull128)

  -- We now have our separate quotients, now we just have to add them together
  quotientHiFinal <- readSTRef quotientHi
  quotientLoFinal <- readSTRef quotientLo
  pure $ (quotientHiFinal `shiftL` 32) .|. quotientLoFinal

divide128By64PreshiftedReduced :: Word64 -> Word64 -> StrengthReducedU64 -> Word64 -> Word64
divide128By64PreshiftedReduced numeratorHi numeratorLo divisorHi divisorFull = undefined

{-# NOINLINE longDivision #-}
longDivision :: forall s. Sliced SmallArray Word64 -> StrengthReducedU64 -> Mutable SmallArray s Word64 -> ST s ()
longDivision numeratorSlice reducedDivisor quotient = do
  remainder <- newSTRef 0
  let numeratorSliceSize = Contiguous.size numeratorSlice
  for_ [numeratorSliceSize, numeratorSliceSize - 1 .. 0] $ \i -> do
    let numeratorElement = Contiguous.index numeratorSlice i
    quotientElement <- Contiguous.read quotient i
    remainder' <- readSTRef remainder
    if remainder' > 0 
    then do
      -- Do one division that includes the running remainder and the upper half of this numerator element, 
      -- then a second division for the first division's remainder combinedwith the lower half
      let upperNumerator = (remainder' `shiftL` 32) .|. (numeratorElement `shiftR` 32)
      let (upperQuotient, upperRemainder) = divRem (upperNumerator, reducedDivisor)

      let lowerNumerator = (upperRemainder `shiftL` 32) .|. (0x00000000_ffffffff .&. numeratorElement)
      let (lowerQuotient, lowerRemainder) = divRem (lowerNumerator, reducedDivisor)

      Contiguous.write quotient i $ (upperQuotient `shiftL` 32) .|. lowerQuotient
      writeSTRef remainder lowerRemainder
    else do
      -- The remainder is zero, which means we can take a shortcut and only do a single division!
      let (digitQuotient, digitRemainder) = divRem (numeratorElement, reducedDivisor)

      Contiguous.write quotient i digitQuotient
      writeSTRef remainder digitRemainder

divide128MaxBy64 :: Word64 -> Word128
divide128MaxBy64 divisor = undefined

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
  where 
  go = do
    x <- p
    if x
    then f *> go
    else pure ()

divRem = undefined

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# LANGUAGE NumericUnderscores #-}

module Atrophy.LongDivision 
  ( module X
  , module Atrophy.LongDivision
  )
  where

import Data.WideWord.Word128
import Data.Word
import Atrophy.Internal.LongDivision as X
import Atrophy.Internal
import qualified Data.Primitive.Contiguous as Contiguous
import Data.Primitive.Contiguous (SmallArray, Slice, Mutable, MutableSliced, Sliced)
import Control.Monad.ST.Strict (ST, runST)
import Data.STRef.Strict (newSTRef, modifySTRef, readSTRef, writeSTRef)
import Data.Bits
import Data.Foldable (for_)
import Data.Functor ((<&>))

divide128By64PreshiftedReduced :: Word64 -> Word64 -> StrengthReducedW64 -> Word64 -> Word64
divide128By64PreshiftedReduced numeratorHi numeratorLo divisorHi divisorFull = undefined

{-# NOINLINE longDivision #-}
longDivision :: forall s. Sliced SmallArray Word64 -> StrengthReducedW64 -> Mutable SmallArray s Word64 -> ST s ()
longDivision numeratorSlice reducedDivisor quotient = do
  remainder <- newSTRef 0
  let numeratorSliceSize = Contiguous.size numeratorSlice
  for_ [numeratorSliceSize - 1, numeratorSliceSize - 2 .. 0] $ \i -> do
    let numeratorElement = Contiguous.index numeratorSlice i
    quotientElement <- Contiguous.read quotient i
    remainder' <- readSTRef remainder
    if remainder' > 0 
    then do
      -- Do one division that includes the running remainder and the upper half of this numerator element, 
      -- then a second division for the first division's remainder combinedwith the lower half
      let upperNumerator = (remainder' `shiftL` 32) .|. (numeratorElement `shiftR` 32)
      let (upperQuotient, upperRemainder) = divRem upperNumerator reducedDivisor

      let lowerNumerator = (upperRemainder `shiftL` 32) .|. (0x00000000_ffffffff .&. numeratorElement)
      let (lowerQuotient, lowerRemainder) = divRem lowerNumerator reducedDivisor

      Contiguous.write quotient i $ (upperQuotient `shiftL` 32) .|. lowerQuotient
      writeSTRef remainder lowerRemainder
    else do
      -- The remainder is zero, which means we can take a shortcut and only do a single division!
      let (digitQuotient, digitRemainder) = divRem numeratorElement reducedDivisor

      Contiguous.write quotient i digitQuotient
      writeSTRef remainder digitRemainder

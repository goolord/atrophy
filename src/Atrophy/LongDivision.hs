{-# LANGUAGE
    TypeApplications
  , ScopedTypeVariables
  , LambdaCase
  , NumericUnderscores
#-}

module Atrophy.LongDivision
  ( module X
  , module Atrophy.LongDivision
  )
  where

import Data.Word
import Atrophy.Internal.LongDivision as X
import Atrophy.Internal
import qualified Data.Primitive.Contiguous as Contiguous
import Data.Primitive.Contiguous (PrimArray, Mutable, Sliced)
import Control.Monad.ST.Strict (ST)
import Data.STRef.Strict (newSTRef, readSTRef, writeSTRef)
import Data.Bits

{-# NOINLINE longDivision #-}
longDivision :: forall s. Sliced PrimArray Word64 -> StrengthReducedW64 -> Mutable PrimArray s Word64 -> ST s ()
longDivision numeratorSlice reducedDivisor quotient = do
  remainder <- newSTRef 0
  (flip Contiguous.itraverse_) numeratorSlice $ \i numerator -> do
    readSTRef remainder >>= \case
      0 -> do
        -- The remainder is zero, which means we can take a shortcut and only do a single division!
        let (digitQuotient, digitRemainder) = divRem numerator reducedDivisor

        Contiguous.write quotient i digitQuotient
        writeSTRef remainder digitRemainder

      remainder' -> do
        -- Do one division that includes the running remainder and the upper half of this numerator element,
        -- then a second division for the first division's remainder combinedwith the lower half
        let upperNumerator = (remainder' `unsafeShiftL` 32) .|. (numerator `unsafeShiftR` 32)
        let (upperQuotient, upperRemainder) = divRem upperNumerator reducedDivisor

        let lowerNumerator = (upperRemainder `unsafeShiftL` 32) .|. (0x00000000_ffffffff .&. numerator)
        let (lowerQuotient, lowerRemainder) = divRem lowerNumerator reducedDivisor

        Contiguous.write quotient i $ (upperQuotient `unsafeShiftL` 32) .|. lowerQuotient
        writeSTRef remainder lowerRemainder

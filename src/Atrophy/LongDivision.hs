{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Dividing multi-limb numbers by a single 64-bit limb.
--
-- Uses Möller & Granlund, "Improved division by invariant integers": a 128/64
-- division costs one multiplication-and-a-half and a predictable branch, given
-- a reciprocal precomputed with a single hardware division.
--
-- Limbs are little-endian: index 0 is the least significant.
module Atrophy.LongDivision
  ( Divisor2By1
  , newDivisor2By1
  , divisor2By1
  , divRem2By1
  , longDivision
  , longDivisionInPlace
  ) where

import Atrophy.Internal (NonZero (..))
import Atrophy.Internal.Prim
import Control.DeepSeq (NFData (..))
import Control.Monad.ST (ST)
import Data.Bits
import Data.Primitive.PrimArray
import Data.Word

-- | A precomputed 64-bit divisor for 128/64 division.
data Divisor2By1 = Divisor2By1
  {-# UNPACK #-} !Word64 -- normalized divisor, top bit set
  {-# UNPACK #-} !Word64 -- reciprocal: floor ((2^128 - 1) / dn) - 2^64
  {-# UNPACK #-} !Int    -- normalization shift
  deriving stock (Eq, Show)

instance NFData Divisor2By1 where
  rnf !_ = ()

{-# INLINE newDivisor2By1 #-}
newDivisor2By1 :: NonZero Word64 -> Divisor2By1
newDivisor2By1 (NonZero d) =
  let !s = countLeadingZeros d
      !dn = unsafeShiftL d s
  in case quotRem128By64 (complement dn) maxBound dn of
    (# v, _ #) -> Divisor2By1 dn v s

-- | The original divisor.
{-# INLINE divisor2By1 #-}
divisor2By1 :: Divisor2By1 -> Word64
divisor2By1 (Divisor2By1 dn _ s) = unsafeShiftR dn s

-- | Möller-Granlund algorithm 4, on a normalized divisor. Requires @u1 < dn@.
{-# INLINE udivrem2By1 #-}
udivrem2By1 :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
udivrem2By1 u1 u0 dn v =
  case mulFull64 v u1 of { (# p1, p0 #) ->
  case addCarry64 p0 u0 of { (# q0, c #) ->
  let !q1 = p1 + u1 + c + 1
      !r = u0 - q1 * dn
      -- if r > q0 then (q1 - 1, r + dn), branchless since it is unpredictable
      !mask = negate (ltW q0 r)
      !q1' = q1 + mask
      !r' = r + (mask .&. dn)
  in if r' >= dn
    then (# q1' + 1, r' - dn #)
    else (# q1', r' #) }}

-- | @divRem2By1 d hi lo@ divides @hi * 2^64 + lo@ by @d@. Requires @hi < d@.
{-# INLINE divRem2By1 #-}
divRem2By1 :: Divisor2By1 -> Word64 -> Word64 -> (Word64, Word64)
divRem2By1 (Divisor2By1 dn v s) hi lo =
  case shl128Small hi lo s of
    (# u1, u0 #) -> case udivrem2By1 u1 u0 dn v of
      (# q, r #) -> (q, unsafeShiftR r s)

-- | Divide the little-endian number in the first argument, writing the
-- quotient limbs to the mutable array, which must be at least as long.
-- Returns the remainder.
longDivision :: Divisor2By1 -> PrimArray Word64 -> MutablePrimArray s Word64 -> ST s Word64
longDivision d numerator quotient = do
  let !len = sizeofPrimArray numerator
  qlen <- getSizeofMutablePrimArray quotient
  if qlen < len
    then error "Atrophy.LongDivision.longDivision: quotient array is too small"
    else do
      copyPrimArray quotient 0 numerator 0 len
      divInPlace d quotient len

-- | Divide the little-endian number in place, replacing it with the quotient.
-- Returns the remainder.
longDivisionInPlace :: Divisor2By1 -> MutablePrimArray s Word64 -> ST s Word64
longDivisionInPlace d arr = getSizeofMutablePrimArray arr >>= divInPlace d arr

{-# INLINE divInPlace #-}
divInPlace :: Divisor2By1 -> MutablePrimArray s Word64 -> Int -> ST s Word64
divInPlace (Divisor2By1 dn v s) arr len
  | len == 0 = pure 0
  | otherwise = do
      -- Normalize in a separate pass rather than on the fly: two tight loops
      -- beat one loop juggling shifts, since GHC's register allocator spills.
      r0 <- if s == 0 then pure 0 else shiftLeftInPlace arr len s
      let go !i !r
            | i < 0 = pure (unsafeShiftR r s)
            | otherwise = do
                u0 <- readPrimArray arr i
                case udivrem2By1 r u0 dn v of
                  (# q, r' #) -> do
                    writePrimArray arr i q
                    go (i - 1) r'
      go (len - 1) r0

-- | Shift the little-endian number left by @0 < s < 64@, returning the bits
-- shifted out of the top.
shiftLeftInPlace :: MutablePrimArray s Word64 -> Int -> Int -> ST s Word64
shiftLeftInPlace arr len s = do
  top <- readPrimArray arr (len - 1)
  let !rs = 64 - s
      go !i !cur
        | i == 0 = writePrimArray arr 0 (unsafeShiftL cur s)
        | otherwise = do
            next <- readPrimArray arr (i - 1)
            writePrimArray arr i (unsafeShiftL cur s .|. unsafeShiftR next rs)
            go (i - 1) next
  go (len - 1) top
  pure (unsafeShiftR top rs)

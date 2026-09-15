{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Multi-limb multiplication. Limbs are little-endian: index 0 is the least
-- significant.
module Atrophy.LongMultiplication
  ( multiply256By128UpperBits
  , longMultiply
  ) where

import Atrophy.Internal.Prim
import Control.Monad.ST (ST)
import Data.Primitive.PrimArray
import Data.WideWord.Word128 (Word128 (..))
import Data.Word

-- | @multiply256By128UpperBits aHi aLo b@ is bits 256 to 383 of
-- @(aHi * 2^128 + aLo) * b@.
{-# INLINE multiply256By128UpperBits #-}
multiply256By128UpperBits :: Word128 -> Word128 -> Word128 -> Word128
multiply256By128UpperBits (Word128 a3 a2) (Word128 a1 a0) (Word128 b1 b0) =
  -- a * b0, limbs 0..4
  case mulFull64 a0 b0 of { (# h0, _ #) ->
  case mulFull64 a1 b0 of { (# h1, l1 #) ->
  case mulFull64 a2 b0 of { (# h2, l2 #) ->
  case mulFull64 a3 b0 of { (# h3, l3 #) ->
  case addCarry64 l1 h0 of { (# x1, c1 #) ->
  case adc64 l2 h1 c1 of { (# x2, c2 #) ->
  case adc64 l3 h2 c2 of { (# x3, c3 #) ->
  let !x4 = h3 + c3 in
  -- a * b1, limbs 1..5
  case mulFull64 a0 b1 of { (# g0, y1 #) ->
  case mulFull64 a1 b1 of { (# g1, k1 #) ->
  case mulFull64 a2 b1 of { (# g2, k2 #) ->
  case mulFull64 a3 b1 of { (# g3, k3 #) ->
  case addCarry64 k1 g0 of { (# y2, e2 #) ->
  case adc64 k2 g1 e2 of { (# y3, e3 #) ->
  case adc64 k3 g2 e3 of { (# y4, e4 #) ->
  let !y5 = g3 + e4 in
  -- sum
  case addCarry64 x1 y1 of { (# _, s1 #) ->
  case adc64 x2 y2 s1 of { (# _, s2 #) ->
  case adc64 x3 y3 s2 of { (# _, s3 #) ->
  case adc64 x4 y4 s3 of { (# r4, s4 #) ->
  Word128 (y5 + s4) r4 }}}}}}}}}}}}}}}}}}

-- | @longMultiply a b prod@ computes @prod += a * b@. The product array must be
-- at least as long as @a@; carries propagate through the rest of it, and
-- running off the end is an error.
longMultiply :: PrimArray Word64 -> Word64 -> MutablePrimArray s Word64 -> ST s ()
longMultiply _ 0 _ = pure ()
longMultiply a b prod = do
  plen <- getSizeofMutablePrimArray prod
  let !alen = sizeofPrimArray a
      mulLoop !i !carry
        | i == alen = carryLoop i carry
        | otherwise = do
            p <- readPrimArray prod i
            case mulFull64 (indexPrimArray a i) b of
              (# h, l #) -> case addCarry64 l p of
                (# s1, c1 #) -> case addCarry64 s1 carry of
                  (# s, c2 #) -> do
                    writePrimArray prod i s
                    mulLoop (i + 1) (h + c1 + c2)
      carryLoop !i !carry
        | carry == 0 = pure ()
        | i == plen = error "Atrophy.LongMultiplication.longMultiply: carry overflow"
        | otherwise = do
            p <- readPrimArray prod i
            case addCarry64 p carry of
              (# s, c #) -> do
                writePrimArray prod i s
                carryLoop (i + 1) c
  if plen < alen
    then error "Atrophy.LongMultiplication.longMultiply: product array is too small"
    else mulLoop 0 0

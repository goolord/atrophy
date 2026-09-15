{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK hide #-}

#include "MachDeps.h"

-- | Double-word and multi-limb primitives. Everything in here is @INLINE@ and
-- is expected to compile down to a handful of instructions. Limbs are passed
-- most significant first, results are returned as unboxed tuples so that
-- nothing is ever allocated.
module Atrophy.Internal.Prim
  ( -- * 64-bit
    mulHi64
  , mulFull64
  , quotRem128By64
  , quotRemWord64
  , quotWord32
  , ltW
  , addCarry64
  , subBorrow64
  , adc64
    -- * 128-bit, as pairs of limbs
  , add128
  , sub128
  , shr128
  , shr128Small
  , shl128Small
  , clz128
  , mulLo128
  , mulHi128
  , mulHi128By64
  , div3By2
    -- * Type level
  , natWord64
  , natInt
  ) where

import Data.Bits
import GHC.Exts
import GHC.TypeNats (KnownNat, natVal')
import GHC.Word

#if WORD_SIZE_IN_BITS == 64

-- | High 64 bits of the 128-bit product. A single @mul@.
{-# INLINE mulHi64 #-}
mulHi64 :: Word64 -> Word64 -> Word64
mulHi64 (W64# a) (W64# b) = case timesWord2# (word64ToWord# a) (word64ToWord# b) of
  (# h, _ #) -> W64# (wordToWord64# h)

-- | @(# hi, lo #)@ of the 128-bit product. A single @mul@.
{-# INLINE mulFull64 #-}
mulFull64 :: Word64 -> Word64 -> (# Word64, Word64 #)
mulFull64 (W64# a) (W64# b) = case timesWord2# (word64ToWord# a) (word64ToWord# b) of
  (# h, l #) -> (# W64# (wordToWord64# h), W64# (wordToWord64# l) #)

-- | @quotRem128By64 hi lo d@ divides @hi * 2^64 + lo@ by @d@. Requires @hi < d@.
-- A single @div@.
{-# INLINE quotRem128By64 #-}
quotRem128By64 :: Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
quotRem128By64 (W64# h) (W64# l) (W64# d) =
  case quotRemWord2# (word64ToWord# h) (word64ToWord# l) (word64ToWord# d) of
    (# q, r #) -> (# W64# (wordToWord64# q), W64# (wordToWord64# r) #)

-- | Unchecked division, the divisor must not be zero.
{-# INLINE quotRemWord64 #-}
quotRemWord64 :: Word64 -> Word64 -> (# Word64, Word64 #)
quotRemWord64 (W64# n) (W64# d) = case quotRemWord# (word64ToWord# n) (word64ToWord# d) of
  (# q, r #) -> (# W64# (wordToWord64# q), W64# (wordToWord64# r) #)

-- | 1 if @a < b@, 0 otherwise. Branchless.
{-# INLINE ltW #-}
ltW :: Word64 -> Word64 -> Word64
ltW (W64# a) (W64# b) = W64# (wordToWord64# (int2Word# (ltWord# (word64ToWord# a) (word64ToWord# b))))

#else

{-# INLINE mulHi64 #-}
mulHi64 :: Word64 -> Word64 -> Word64
mulHi64 a b = case mulFull64 a b of (# h, _ #) -> h

{-# INLINE mulFull64 #-}
mulFull64 :: Word64 -> Word64 -> (# Word64, Word64 #)
mulFull64 a b =
  let !aL = a .&. 0xffffffff
      !aH = a `unsafeShiftR` 32
      !bL = b .&. 0xffffffff
      !bH = b `unsafeShiftR` 32
      !ll = aL * bL
      !lh = aL * bH
      !hl = aH * bL
      !hh = aH * bH
      !mid = (ll `unsafeShiftR` 32) + (lh .&. 0xffffffff) + (hl .&. 0xffffffff)
  in (# hh + (lh `unsafeShiftR` 32) + (hl `unsafeShiftR` 32) + (mid `unsafeShiftR` 32), a * b #)

{-# INLINE quotRem128By64 #-}
quotRem128By64 :: Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
quotRem128By64 h l d =
  case (toInteger h `unsafeShiftL` 64 .|. toInteger l) `quotRem` toInteger d of
    (q, r) -> (# fromInteger q, fromInteger r #)

{-# INLINE quotRemWord64 #-}
quotRemWord64 :: Word64 -> Word64 -> (# Word64, Word64 #)
quotRemWord64 n d = case quotRem n d of (q, r) -> (# q, r #)

{-# INLINE ltW #-}
ltW :: Word64 -> Word64 -> Word64
ltW a b = if a < b then 1 else 0

#endif

-- | Unchecked division, the divisor must not be zero.
{-# INLINE quotWord32 #-}
quotWord32 :: Word32 -> Word32 -> Word32
quotWord32 (W32# n) (W32# d) = W32# (quotWord32# n d)

-- | @(# sum, carry #)@
{-# INLINE addCarry64 #-}
addCarry64 :: Word64 -> Word64 -> (# Word64, Word64 #)
addCarry64 a b = let !s = a + b in (# s, ltW s a #)

-- | @(# difference, borrow #)@
{-# INLINE subBorrow64 #-}
subBorrow64 :: Word64 -> Word64 -> (# Word64, Word64 #)
subBorrow64 a b = (# a - b, ltW a b #)

-- | @a + b + carry@, returning @(# sum, carry #)@. @carry@ must be 0 or 1.
{-# INLINE adc64 #-}
adc64 :: Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
adc64 a b c =
  let !s1 = a + b
      !s = s1 + c
  in (# s, ltW s1 a + ltW s s1 #)

{-# INLINE add128 #-}
add128 :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
add128 a1 a0 b1 b0 = case addCarry64 a0 b0 of (# s, c #) -> (# a1 + b1 + c, s #)

{-# INLINE sub128 #-}
sub128 :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
sub128 a1 a0 b1 b0 = case subBorrow64 a0 b0 of (# s, c #) -> (# a1 - b1 - c, s #)

-- | Shift right by @0 <= s < 64@, branchless.
{-# INLINE shr128Small #-}
shr128Small :: Word64 -> Word64 -> Int -> (# Word64, Word64 #)
shr128Small x1 x0 s =
  (# x1 `unsafeShiftR` s
  , (x0 `unsafeShiftR` s) .|. ((x1 `unsafeShiftL` 1) `unsafeShiftL` (63 - s))
  #)

-- | Shift left by @0 <= s < 64@, branchless.
{-# INLINE shl128Small #-}
shl128Small :: Word64 -> Word64 -> Int -> (# Word64, Word64 #)
shl128Small x1 x0 s =
  (# (x1 `unsafeShiftL` s) .|. ((x0 `unsafeShiftR` 1) `unsafeShiftR` (63 - s))
  , x0 `unsafeShiftL` s
  #)

-- | Shift right by @0 <= s < 128@.
{-# INLINE shr128 #-}
shr128 :: Word64 -> Word64 -> Int -> (# Word64, Word64 #)
shr128 x1 x0 s
  | s < 64 = shr128Small x1 x0 s
  | otherwise = (# 0, x1 `unsafeShiftR` (s - 64) #)

{-# INLINE clz128 #-}
clz128 :: Word64 -> Word64 -> Int
clz128 x1 x0 = if x1 == 0 then 64 + countLeadingZeros x0 else countLeadingZeros x1

-- | Low 128 bits of the product.
{-# INLINE mulLo128 #-}
mulLo128 :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
mulLo128 a1 a0 b1 b0 = case mulFull64 a0 b0 of
  (# h, l #) -> (# h + a0 * b1 + a1 * b0, l #)

-- | High 128 bits of the 256-bit product.
{-# INLINE mulHi128 #-}
mulHi128 :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
mulHi128 a1 a0 b1 b0 =
  case mulFull64 a0 b0 of { (# h00, _ #) ->
  case mulFull64 a0 b1 of { (# h01, l01 #) ->
  case mulFull64 a1 b0 of { (# h10, l10 #) ->
  case mulFull64 a1 b1 of { (# h11, l11 #) ->
  case addCarry64 h00 l01 of { (# s1, c1a #) ->
  case addCarry64 s1 l10 of { (# _, c1b #) ->
  case addCarry64 h01 h10 of { (# s2a, c2a #) ->
  case addCarry64 s2a l11 of { (# s2b, c2b #) ->
  case addCarry64 s2b (c1a + c1b) of { (# s2, c2c #) ->
  (# h11 + c2a + c2b + c2c, s2 #) }}}}}}}}}

-- | High 128 bits of the 192-bit product of a 128-bit and a 64-bit number.
{-# INLINE mulHi128By64 #-}
mulHi128By64 :: Word64 -> Word64 -> Word64 -> (# Word64, Word64 #)
mulHi128By64 a1 a0 b =
  case mulFull64 a1 b of { (# h1, l1 #) ->
  case mulHi64 a0 b of { h0 ->
  case addCarry64 l1 h0 of { (# s, c #) ->
  (# h1 + c, s #) }}}

-- | Divide the 192-bit @(u2, u1, u0)@ by the normalized (top bit set) 128-bit
-- @(d1, d0)@, given @(u2, u1) < (d1, d0)@. The quotient fits in 64 bits.
-- Returns @(# quotient, remainder hi, remainder lo #)@.
--
-- Knuth's algorithm D, one step: estimate from the top limbs with a hardware
-- division, then correct at most twice.
{-# INLINE div3By2 #-}
div3By2 :: Word64 -> Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64, Word64 #)
div3By2 u2 u1 u0 d1 d0 =
  let !qhat = if u2 >= d1 then maxBound else case quotRem128By64 u2 u1 d1 of (# q, _ #) -> q
  in
  case mulFull64 qhat d0 of { (# a1, a0 #) ->
  case mulFull64 qhat d1 of { (# b1, b0 #) ->
  case addCarry64 b0 a1 of { (# p1, c #) ->
  correct qhat (b1 + c) p1 a0 }}}
  where
  correct :: Word64 -> Word64 -> Word64 -> Word64 -> (# Word64, Word64, Word64 #)
  correct !q !p2 !p1 !p0
    | p2 > u2 || (p2 == u2 && (p1 > u1 || (p1 == u1 && p0 > u0))) =
        case subBorrow64 p0 d0 of { (# p0', b0 #) ->
        case subBorrow64 p1 d1 of { (# t1, b1a #) ->
        case subBorrow64 t1 b0 of { (# p1', b1b #) ->
        correct (q - 1) (p2 - b1a - b1b) p1' p0' }}}
    | otherwise =
        -- the remainder is below d, so its top limb is zero
        case subBorrow64 u0 p0 of { (# r0, b0 #) ->
        (# q, u1 - p1 - b0, r0 #) }

-- | A type level natural as a 'Word64' literal. Folds at compile time.
{-# INLINE natWord64 #-}
natWord64 :: forall n. KnownNat n => Word64
natWord64 = fromIntegral (natVal' (proxy# :: Proxy# n))

-- | A type level natural as an 'Int' literal. Folds at compile time.
{-# INLINE natInt #-}
natInt :: forall n. KnownNat n => Int
natInt = fromIntegral (natVal' (proxy# :: Proxy# n))

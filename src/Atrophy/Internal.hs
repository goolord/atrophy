{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_HADDOCK not-home #-}

module Atrophy.Internal where

import Atrophy.Internal.Prim
import Control.DeepSeq (NFData (..))
import Data.Bits
import Data.Kind (Type)
import Data.WideWord.Word128 (Word128 (..))
import Data.Word

-- | A value that is known not to be zero.
newtype NonZero a = NonZero a
  deriving newtype (Eq, Ord, Show, Num, NFData)

instance (Bounded a, Num a) => Bounded (NonZero a) where
  minBound = NonZero 1
  maxBound = NonZero maxBound

{-# INLINE getNonZero #-}
getNonZero :: NonZero a -> a
getNonZero (NonZero a) = a

-- | 'Nothing' for zero.
{-# INLINE nonZero #-}
nonZero :: (Eq a, Num a) => a -> Maybe (NonZero a)
nonZero 0 = Nothing
nonZero a = Just (NonZero a)

-- | Unsigned integers that can be divided by a precomputed divisor.
--
-- Every method is @INLINE@; calls at a known type compile down to a handful of
-- instructions with no dictionary passing and no allocation.
class StrengthReduce a where
  -- | The precomputed form of a divisor.
  type StrengthReduced a = (r :: Type) | r -> a

  -- | Precompute a divisor. This costs about as much as one hardware division.
  new :: NonZero a -> StrengthReduced a

  -- | The original divisor.
  divisor :: StrengthReduced a -> a

  -- | Quotient and remainder, branchless.
  divRem :: a -> StrengthReduced a -> (a, a)

  -- | 'divRem', specialised for numerators that are compile-time constants.
  --
  -- The result is identical to 'divRem', but the implementation branches on
  -- properties of the numerator (zero, a power of two, all ones, fits in half
  -- the width). When the numerator is a literal, GHC folds those branches away
  -- and the multiplication often turns into a shift or disappears entirely.
  -- When it is not a literal you pay for the branches, so use 'divRem'.
  --
  -- See also 'Atrophy.Known.divRemN', which takes the numerator at the type
  -- level and so is guaranteed to be constant.
  divRemConst :: a -> StrengthReduced a -> (a, a)
  divRemConst = divRem
  {-# INLINE divRemConst #-}

  -- | Hardware division without the zero check performed by 'quotRem'.
  divRemNonZero :: a -> NonZero a -> (a, a)

  -- | 'divRemNonZero', specialised for numerators that are compile-time
  -- constants, e.g. using a narrower hardware division when the numerator is
  -- small. The same caveats as 'divRemConst' apply.
  divRemNonZeroConst :: a -> NonZero a -> (a, a)
  divRemNonZeroConst = divRemNonZero
  {-# INLINE divRemNonZeroConst #-}

-- | Quotient by a precomputed divisor. The remainder is never computed.
{-# INLINE div' #-}
div' :: StrengthReduce a => a -> StrengthReduced a -> a
div' n d = case divRem n d of (q, _) -> q

-- | Remainder by a precomputed divisor.
{-# INLINE rem' #-}
rem' :: StrengthReduce a => a -> StrengthReduced a -> a
rem' n d = case divRem n d of (_, r) -> r

-- | Quotient of a compile-time constant numerator. See 'divRemConst'.
{-# INLINE divConst #-}
divConst :: StrengthReduce a => a -> StrengthReduced a -> a
divConst n d = case divRemConst n d of (q, _) -> q

-- | Remainder of a compile-time constant numerator. See 'divRemConst'.
{-# INLINE remConst #-}
remConst :: StrengthReduce a => a -> StrengthReduced a -> a
remConst n d = case divRemConst n d of (_, r) -> r

-- | Unchecked hardware quotient. See 'divRemNonZero'.
{-# INLINE divNonZero #-}
divNonZero :: StrengthReduce a => a -> NonZero a -> a
divNonZero n d = case divRemNonZero n d of (q, _) -> q

-- | Unchecked hardware remainder. See 'divRemNonZero'.
{-# INLINE remNonZero #-}
remNonZero :: StrengthReduce a => a -> NonZero a -> a
remNonZero n d = case divRemNonZero n d of (_, r) -> r

-- | Unchecked quotient of a compile-time constant numerator. See
-- 'divRemNonZeroConst'.
{-# INLINE divNonZeroConst #-}
divNonZeroConst :: StrengthReduce a => a -> NonZero a -> a
divNonZeroConst n d = case divRemNonZeroConst n d of (q, _) -> q

-- | Unchecked remainder of a compile-time constant numerator. See
-- 'divRemNonZeroConst'.
{-# INLINE remNonZeroConst #-}
remNonZeroConst :: StrengthReduce a => a -> NonZero a -> a
remNonZeroConst n d = case divRemNonZeroConst n d of (_, r) -> r

--------------------------------------------------------------------------------
-- Word64: Granlund-Montgomery, "Division by Invariant Integers using
-- Multiplication", figure 4.1. One multiplication, fully branchless, and
-- handles 1 and powers of two without special cases.
--
--   l  = ceil (log2 d)
--   m  = floor (2^64 * (2^l - d) / d) + 1
--   t  = mulhi m n
--   q  = (t + ((n - t) >> min l 1)) >> max (l - 1) 0
--------------------------------------------------------------------------------

-- | A precomputed 'Word64' divisor. Four words.
data StrengthReducedW64 = StrengthReducedW64
  {-# UNPACK #-} !Word64 -- multiplier
  {-# UNPACK #-} !Int    -- pre-shift, 0 or 1
  {-# UNPACK #-} !Int    -- post-shift
  {-# UNPACK #-} !Word64 -- divisor
  deriving stock (Eq, Show)

instance NFData StrengthReducedW64 where
  rnf !_ = ()

instance StrengthReduce Word64 where
  type StrengthReduced Word64 = StrengthReducedW64

  {-# INLINE new #-}
  new (NonZero d) =
    let !l = 64 - countLeadingZeros (d - 1)
        !h = if l == 64 then negate d else unsafeShiftL 1 l - d
        !m = if h == 0 then 1 else case quotRem128By64 h 0 d of (# q, _ #) -> q + 1
    in StrengthReducedW64 m (min l 1) (max (l - 1) 0) d

  {-# INLINE divisor #-}
  divisor (StrengthReducedW64 _ _ _ d) = d

  {-# INLINE divRem #-}
  divRem n (StrengthReducedW64 m s1 s2 d) = gm64 n (mulHi64 m n) s1 s2 d

  {-# INLINE divRemConst #-}
  divRemConst n (StrengthReducedW64 m s1 s2 d)
    | n == 0 = (0, 0)
    | n == 1 = gm64 n 0 s1 s2 d
    -- mulhi m (2^64 - 1) == m - 1, since 1 <= m < 2^64
    | n == maxBound = gm64 n (m - 1) s1 s2 d
    -- mulhi m (2^k) == m >> (64 - k)
    | popCount n == 1 = gm64 n (unsafeShiftR m (64 - countTrailingZeros n)) s1 s2 d
    | otherwise = gm64 n (mulHi64 m n) s1 s2 d

  {-# INLINE divRemNonZero #-}
  divRemNonZero n (NonZero d) = case quotRemWord64 n d of (# q, r #) -> (q, r)

  {-# INLINE divRemNonZeroConst #-}
  divRemNonZeroConst n (NonZero d)
    | n == 0 = (0, 0)
    -- a 32-bit hardware division is considerably cheaper on many CPUs
    | n <= 0xffffffff =
        if d > n
          then (0, n)
          else
            let !q = fromIntegral (quotWord32 (fromIntegral n) (fromIntegral d))
            in (q, n - q * d)
    | otherwise = case quotRemWord64 n d of (# q, r #) -> (q, r)

-- | The part of Granlund-Montgomery after the multiplication.
{-# INLINE gm64 #-}
gm64 :: Word64 -> Word64 -> Int -> Int -> Word64 -> (Word64, Word64)
gm64 n t s1 s2 d =
  let !q = (t + unsafeShiftR (n - t) s1) `unsafeShiftR` s2
  in (q, n - q * d)

--------------------------------------------------------------------------------
-- Word32 and smaller: Lemire, Kaser & Kurz, "Faster Remainder by Direct
-- Computation". With a 64-bit reciprocal c = ceil (2^64 / d) the quotient of
-- any 32-bit n is exactly mulhi c n. The reciprocal wraps to 0 for d == 1, so
-- a mask adds n back in that case.
--------------------------------------------------------------------------------

{-# INLINE newSmall #-}
newSmall :: Word64 -> (# Word64, Word64 #)
newSmall d = case quotRemWord64 maxBound d of
  (# q, _ #) -> (# q + 1, negate (ltW d 2) #)

{-# INLINE quotSmall #-}
quotSmall :: Word64 -> Word64 -> Word64 -> Word64
quotSmall c mask n = mulHi64 c n + (n .&. mask)

{-# INLINE quotSmallConst #-}
quotSmallConst :: Word64 -> Word64 -> Word64 -> Word64
quotSmallConst c mask n
  | n == 0 = 0
  | n == 1 = mask .&. 1
  -- mulhi c (2^k) == c >> (64 - k)
  | popCount n == 1 = unsafeShiftR c (64 - countTrailingZeros n) + (n .&. mask)
  | otherwise = quotSmall c mask n

-- | A precomputed 'Word32' divisor.
data StrengthReducedW32 = StrengthReducedW32
  {-# UNPACK #-} !Word64 -- reciprocal
  {-# UNPACK #-} !Word64 -- all ones when the divisor is 1
  {-# UNPACK #-} !Word32 -- divisor
  deriving stock (Eq, Show)

instance NFData StrengthReducedW32 where
  rnf !_ = ()

instance StrengthReduce Word32 where
  type StrengthReduced Word32 = StrengthReducedW32

  {-# INLINE new #-}
  new (NonZero d) = case newSmall (fromIntegral d) of
    (# c, mask #) -> StrengthReducedW32 c mask d

  {-# INLINE divisor #-}
  divisor (StrengthReducedW32 _ _ d) = d

  {-# INLINE divRem #-}
  divRem n (StrengthReducedW32 c mask d) =
    let !q = fromIntegral (quotSmall c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemConst #-}
  divRemConst n (StrengthReducedW32 c mask d) =
    let !q = fromIntegral (quotSmallConst c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemNonZero #-}
  divRemNonZero n (NonZero d) =
    let !q = quotWord32 n d
    in (q, n - q * d)

-- | A precomputed 'Word16' divisor.
data StrengthReducedW16 = StrengthReducedW16
  {-# UNPACK #-} !Word64 -- reciprocal
  {-# UNPACK #-} !Word64 -- all ones when the divisor is 1
  {-# UNPACK #-} !Word16 -- divisor
  deriving stock (Eq, Show)

instance NFData StrengthReducedW16 where
  rnf !_ = ()

instance StrengthReduce Word16 where
  type StrengthReduced Word16 = StrengthReducedW16

  {-# INLINE new #-}
  new (NonZero d) = case newSmall (fromIntegral d) of
    (# c, mask #) -> StrengthReducedW16 c mask d

  {-# INLINE divisor #-}
  divisor (StrengthReducedW16 _ _ d) = d

  {-# INLINE divRem #-}
  divRem n (StrengthReducedW16 c mask d) =
    let !q = fromIntegral (quotSmall c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemConst #-}
  divRemConst n (StrengthReducedW16 c mask d) =
    let !q = fromIntegral (quotSmallConst c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemNonZero #-}
  divRemNonZero n (NonZero d) =
    let !q = fromIntegral (quotWord32 (fromIntegral n) (fromIntegral d))
    in (q, n - q * d)

-- | A precomputed 'Word8' divisor.
data StrengthReducedW8 = StrengthReducedW8
  {-# UNPACK #-} !Word64 -- reciprocal
  {-# UNPACK #-} !Word64 -- all ones when the divisor is 1
  {-# UNPACK #-} !Word8  -- divisor
  deriving stock (Eq, Show)

instance NFData StrengthReducedW8 where
  rnf !_ = ()

instance StrengthReduce Word8 where
  type StrengthReduced Word8 = StrengthReducedW8

  {-# INLINE new #-}
  new (NonZero d) = case newSmall (fromIntegral d) of
    (# c, mask #) -> StrengthReducedW8 c mask d

  {-# INLINE divisor #-}
  divisor (StrengthReducedW8 _ _ d) = d

  {-# INLINE divRem #-}
  divRem n (StrengthReducedW8 c mask d) =
    let !q = fromIntegral (quotSmall c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemConst #-}
  divRemConst n (StrengthReducedW8 c mask d) =
    let !q = fromIntegral (quotSmallConst c mask (fromIntegral n))
    in (q, n - q * d)

  {-# INLINE divRemNonZero #-}
  divRemNonZero n (NonZero d) =
    let !q = fromIntegral (quotWord32 (fromIntegral n) (fromIntegral d))
    in (q, n - q * d)

--------------------------------------------------------------------------------
-- Word128: Granlund-Montgomery again, on 64-bit limbs.
--------------------------------------------------------------------------------

-- | A precomputed 'Word128' divisor.
data StrengthReducedW128 = StrengthReducedW128
  {-# UNPACK #-} !Word64 -- multiplier hi
  {-# UNPACK #-} !Word64 -- multiplier lo
  {-# UNPACK #-} !Int    -- pre-shift, 0 or 1
  {-# UNPACK #-} !Int    -- post-shift
  {-# UNPACK #-} !Word64 -- divisor hi
  {-# UNPACK #-} !Word64 -- divisor lo
  deriving stock (Eq, Show)

instance NFData StrengthReducedW128 where
  rnf !_ = ()

instance StrengthReduce Word128 where
  type StrengthReduced Word128 = StrengthReducedW128

  {-# INLINE new #-}
  new = new128

  {-# INLINE divisor #-}
  divisor (StrengthReducedW128 _ _ _ _ d1 d0) = Word128 d1 d0

  {-# INLINE divRem #-}
  divRem (Word128 n1 n0) (StrengthReducedW128 m1 m0 s1 s2 d1 d0) =
    case mulHi128 m1 m0 n1 n0 of (# t1, t0 #) -> gm128 n1 n0 t1 t0 s1 s2 d1 d0

  {-# INLINE divRemConst #-}
  divRemConst (Word128 n1 n0) (StrengthReducedW128 m1 m0 s1 s2 d1 d0)
    | n1 == 0 && n0 == 0 = (Word128 0 0, Word128 0 0)
    | n1 == maxBound && n0 == maxBound =
        case sub128 m1 m0 0 1 of (# t1, t0 #) -> gm128 n1 n0 t1 t0 s1 s2 d1 d0
    | popCount n1 + popCount n0 == 1 =
        let !k = if n1 == 0 then countTrailingZeros n0 else 64 + countTrailingZeros n1
        in if k == 0
          then gm128 n1 n0 0 0 s1 s2 d1 d0
          else case shr128 m1 m0 (128 - k) of (# t1, t0 #) -> gm128 n1 n0 t1 t0 s1 s2 d1 d0
    -- n < 2^64: the multiplication is only 128x64, the quotient and remainder
    -- fit in 64 bits
    | n1 == 0 =
        -- floor (m * n / 2^128) <= n < 2^64, so it is just the top limb
        case mulHi128By64 m1 m0 n0 of
          (# t, _ #) ->
            let !x = t + unsafeShiftR (n0 - t) s1
                !q = if s2 < 64 then unsafeShiftR x s2 else 0
            in (Word128 0 q, Word128 0 (n0 - q * d0))
    | otherwise =
        case mulHi128 m1 m0 n1 n0 of (# t1, t0 #) -> gm128 n1 n0 t1 t0 s1 s2 d1 d0

  {-# INLINE divRemNonZero #-}
  divRemNonZero = divRemNonZero128

  {-# INLINE divRemNonZeroConst #-}
  divRemNonZeroConst n@(Word128 n1 n0) d@(NonZero (Word128 d1 d0))
    | n1 == 0 && n0 == 0 = (Word128 0 0, Word128 0 0)
    | n1 == 0 =
        if d1 /= 0
          then (Word128 0 0, n)
          else case divRemNonZeroConst n0 (NonZero d0) of
            (q, r) -> (Word128 0 q, Word128 0 r)
    | otherwise = divRemNonZero128 n d

{-# INLINE gm128 #-}
gm128 :: Word64 -> Word64 -> Word64 -> Word64 -> Int -> Int -> Word64 -> Word64 -> (Word128, Word128)
gm128 n1 n0 t1 t0 s1 s2 d1 d0 =
  case sub128 n1 n0 t1 t0 of { (# x1, x0 #) ->
  case shr128Small x1 x0 s1 of { (# y1, y0 #) ->
  case add128 y1 y0 t1 t0 of { (# z1, z0 #) ->
  case shr128 z1 z0 s2 of { (# q1, q0 #) ->
  case mulLo128 q1 q0 d1 d0 of { (# p1, p0 #) ->
  case sub128 n1 n0 p1 p0 of { (# r1, r0 #) ->
  (Word128 q1 q0, Word128 r1 r0) }}}}}}

{-# INLINE new128 #-}
new128 :: NonZero Word128 -> StrengthReducedW128
new128 (NonZero (Word128 d1 d0)) =
  case sub128 d1 d0 0 1 of { (# e1, e0 #) ->
  let !l = 128 - clz128 e1 e0
      !s1 = min l 1
      !s2 = max (l - 1) 0
      sr m1 m0 = StrengthReducedW128 m1 m0 s1 s2 d1 d0
  in
  case pow2Minus l of { (# h1, h0 #) ->
  if h1 == 0 && h0 == 0
    then sr 0 1
  else if d1 == 0
    -- h < d < 2^64: two 128/64 hardware divisions
    then
      case quotRem128By64 h0 0 d0 of { (# q1, r1 #) ->
      case quotRem128By64 r1 0 d0 of { (# q0, _ #) ->
      case add128 q1 q0 0 1 of { (# m1, m0 #) ->
      sr m1 m0 }}}
    -- normalize and do two 3-by-2 divisions of (h << s) * 2^128
    else
      let !s = countLeadingZeros d1 in
      case shl128Small d1 d0 s of { (# dn1, dn0 #) ->
      case shl128Small h1 h0 s of { (# u1, u0 #) ->
      case div3By2 u1 u0 0 dn1 dn0 of { (# q1, r1, r0 #) ->
      case div3By2 r1 r0 0 dn1 dn0 of { (# q0, _, _ #) ->
      case add128 q1 q0 0 1 of { (# m1, m0 #) ->
      sr m1 m0 }}}}}
  }}
  where
  -- 2^l - d, modulo 2^128
  pow2Minus :: Int -> (# Word64, Word64 #)
  pow2Minus l
    | l == 128 = sub128 0 0 d1 d0
    | l >= 64 = sub128 (unsafeShiftL 1 (l - 64)) 0 d1 d0
    | otherwise = sub128 0 (unsafeShiftL 1 l) d1 d0

{-# INLINE divRemNonZero128 #-}
divRemNonZero128 :: Word128 -> NonZero Word128 -> (Word128, Word128)
divRemNonZero128 (Word128 n1 n0) (NonZero (Word128 d1 d0))
  | d1 == 0 =
      if n1 < d0
        then case quotRem128By64 n1 n0 d0 of
          (# q, r #) -> (Word128 0 q, Word128 0 r)
        else
          -- NB. not quotRemWord64: when only the remainder is demanded GHC
          -- turns that into a remWord# expression and sinks it into the high
          -- argument of the quotRemWord2# below, and the x86 NCG (9.14 at
          -- least) then clobbers rax while computing it. quotRemWord2# is
          -- never sunk.
          case quotRem128By64 0 n1 d0 of { (# q1, r1 #) ->
          case quotRem128By64 r1 n0 d0 of { (# q0, r #) ->
          (Word128 q1 q0, Word128 0 r) }}
  | otherwise =
      -- the quotient fits in 64 bits: normalize, one 3-by-2 division
      let !s = countLeadingZeros d1
          !u2 = unsafeShiftR (unsafeShiftR n1 1) (63 - s)
      in
      case shl128Small d1 d0 s of { (# dn1, dn0 #) ->
      case shl128Small n1 n0 s of { (# u1, u0 #) ->
      case div3By2 u2 u1 u0 dn1 dn0 of { (# q, r1, r0 #) ->
      case shr128Small r1 r0 s of { (# rr1, rr0 #) ->
      (Word128 0 q, Word128 rr1 rr0) }}}}

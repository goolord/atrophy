{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Division where one side is known at compile time, supplied at the type
-- level so that it is guaranteed to be a constant.
--
-- GHC's native code generator does /not/ strength-reduce division by
-- constants: @x \`quot\` 7@ compiles to a @div@ instruction. 'divK' computes
-- the magic numbers during type checking instead:
--
-- >>> divK @7 (100 :: Word64)
-- 14
--
-- 'divRemN' goes the other way: the divisor is a runtime 'StrengthReduced'
-- value, but the numerator is fixed, which lets e.g. @divN \@(2 ^ 63)@ skip
-- the multiplication entirely.
module Atrophy.Known
  ( -- * Divisors known at compile time
    KnownDivisor (..)
  , divK
  , remK
    -- * Numerators known at compile time
  , KnownNumerator (..)
  , divRemN
  , divN
  , remN
  , divRemNonZeroN
  , divNonZeroN
  , remNonZeroN
  ) where

import Atrophy.Internal
import Atrophy.Internal.Prim
import Data.Bits
import Data.Kind (Constraint, Type)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Data.WideWord.Word128 (Word128 (..))
import Data.Word
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeNats

type Assert :: Bool -> Constraint -> Constraint
type family Assert b err where
  Assert 'True _ = ()
  Assert _ err = err

type CheckDivisor :: Nat -> Nat -> Constraint
type CheckDivisor bits d =
  ( Assert (1 <=? d) (TypeError ('Text "atrophy: division by zero"))
  , Assert (d <=? 2 ^ bits - 1)
      (TypeError ('Text "atrophy: the divisor " ':<>: 'ShowType d ':<>: 'Text " does not fit in " ':<>: 'ShowType bits ':<>: 'Text " bits"))
  )

type CheckNumerator :: Nat -> Nat -> Constraint
type CheckNumerator bits n =
  Assert (n <=? 2 ^ bits - 1)
    (TypeError ('Text "atrophy: the numerator " ':<>: 'ShowType n ':<>: 'Text " does not fit in " ':<>: 'ShowType bits ':<>: 'Text " bits"))

--------------------------------------------------------------------------------
-- Magic numbers, following libdivide's unsigned branching algorithm. Since the
-- choice of algorithm is made at compile time there is no branch at runtime.
--
-- 0: power of two, shift only
-- 1: q = mulhi m n >> s
-- 2: q = (((n - t) >> 1) + t) >> s  where t = mulhi m n
--------------------------------------------------------------------------------

type IsPow2 d = 2 ^ Log2 d == d
type ProposedM bits d = Div (2 ^ (bits + Log2 d)) d
type ProposedR bits d = Mod (2 ^ (bits + Log2 d)) d
type NoAdd bits d = CmpNat (d - ProposedR bits d) (2 ^ Log2 d) == 'LT

type Algo bits d = If (IsPow2 d) 0 (If (NoAdd bits d) 1 2)

type Magic bits d =
  If (IsPow2 d) 0
    (If (NoAdd bits d)
      (ProposedM bits d + 1)
      (Mod (2 * ProposedM bits d + If (d <=? 2 * ProposedR bits d) 1 0 + 1) (2 ^ bits)))

-- | For 32 bits and below: q = mulhi c n, with c = ceil (2^64 / d).
type Lemire d = Div (2 ^ 64) d + 1

-- | Types that can be divided by the type-level constant @d@.
--
-- @divRemK \@d n == n \`quotRem\` d@, but the division is replaced by at most
-- one multiplication and some shifts, with no branches.
type KnownDivisor :: Nat -> Type -> Constraint
class KnownDivisor d a where
  divRemK :: a -> (a, a)

-- | Quotient by the type-level divisor @d@.
{-# INLINE divK #-}
divK :: forall d a. KnownDivisor d a => a -> a
divK n = case divRemK @d n of (q, _) -> q

-- | Remainder by the type-level divisor @d@.
{-# INLINE remK #-}
remK :: forall d a. KnownDivisor d a => a -> a
remK n = case divRemK @d n of (_, r) -> r

instance
  ( CheckDivisor 64 d
  , KnownNat d
  , KnownNat (Log2 d)
  , KnownNat (Algo 64 d)
  , KnownNat (Magic 64 d)
  ) => KnownDivisor d Word64 where
  {-# INLINE divRemK #-}
  divRemK n = case natWord64 @(Algo 64 d) of
    0 -> (unsafeShiftR n sh, n .&. (dv - 1))
    1 -> let !q = unsafeShiftR (mulHi64 mg n) sh in (q, n - q * dv)
    _ -> let !t = mulHi64 mg n
             !q = unsafeShiftR (unsafeShiftR (n - t) 1 + t) sh
         in (q, n - q * dv)
    where
    sh = natInt @(Log2 d)
    mg = natWord64 @(Magic 64 d)
    dv = natWord64 @d

{-# INLINE smallK #-}
smallK :: Word64 -> Int -> Word64 -> Word64 -> Word64 -> (Word64, Word64)
smallK algo sh c dv n = case algo of
  0 -> (unsafeShiftR n sh, n .&. (dv - 1))
  _ -> let !q = mulHi64 c n in (q, n - q * dv)

instance
  ( CheckDivisor 32 d
  , KnownNat d
  , KnownNat (Log2 d)
  , KnownNat (If (IsPow2 d) 0 1)
  , KnownNat (Lemire d)
  ) => KnownDivisor d Word32 where
  {-# INLINE divRemK #-}
  divRemK n =
    case smallK (natWord64 @(If (IsPow2 d) 0 1)) (natInt @(Log2 d)) (natWord64 @(Lemire d)) (natWord64 @d) (fromIntegral n) of
      (q, r) -> (fromIntegral q, fromIntegral r)

instance
  ( CheckDivisor 16 d
  , KnownNat d
  , KnownNat (Log2 d)
  , KnownNat (If (IsPow2 d) 0 1)
  , KnownNat (Lemire d)
  ) => KnownDivisor d Word16 where
  {-# INLINE divRemK #-}
  divRemK n =
    case smallK (natWord64 @(If (IsPow2 d) 0 1)) (natInt @(Log2 d)) (natWord64 @(Lemire d)) (natWord64 @d) (fromIntegral n) of
      (q, r) -> (fromIntegral q, fromIntegral r)

instance
  ( CheckDivisor 8 d
  , KnownNat d
  , KnownNat (Log2 d)
  , KnownNat (If (IsPow2 d) 0 1)
  , KnownNat (Lemire d)
  ) => KnownDivisor d Word8 where
  {-# INLINE divRemK #-}
  divRemK n =
    case smallK (natWord64 @(If (IsPow2 d) 0 1)) (natInt @(Log2 d)) (natWord64 @(Lemire d)) (natWord64 @d) (fromIntegral n) of
      (q, r) -> (fromIntegral q, fromIntegral r)

instance
  ( CheckDivisor 128 d
  , KnownNat (Log2 d)
  , KnownNat (Algo 128 d)
  , KnownNat (Div (Magic 128 d) (2 ^ 64))
  , KnownNat (Mod (Magic 128 d) (2 ^ 64))
  , KnownNat (Div d (2 ^ 64))
  , KnownNat (Mod d (2 ^ 64))
  ) => KnownDivisor d Word128 where
  {-# INLINE divRemK #-}
  divRemK (Word128 n1 n0) = case natWord64 @(Algo 128 d) of
    0 ->
      case shr128 n1 n0 sh of { (# q1, q0 #) ->
      case sub128 d1 d0 0 1 of { (# e1, e0 #) ->
      (Word128 q1 q0, Word128 (n1 .&. e1) (n0 .&. e0)) }}
    1 ->
      case mulHi128 m1 m0 n1 n0 of { (# t1, t0 #) ->
      case shr128 t1 t0 sh of { (# q1, q0 #) ->
      finish q1 q0 }}
    _ ->
      case mulHi128 m1 m0 n1 n0 of { (# t1, t0 #) ->
      case sub128 n1 n0 t1 t0 of { (# x1, x0 #) ->
      case shr128Small x1 x0 1 of { (# y1, y0 #) ->
      case add128 y1 y0 t1 t0 of { (# z1, z0 #) ->
      case shr128 z1 z0 sh of { (# q1, q0 #) ->
      finish q1 q0 }}}}}
    where
    sh = natInt @(Log2 d)
    m1 = natWord64 @(Div (Magic 128 d) (2 ^ 64))
    m0 = natWord64 @(Mod (Magic 128 d) (2 ^ 64))
    d1 = natWord64 @(Div d (2 ^ 64))
    d0 = natWord64 @(Mod d (2 ^ 64))
    finish q1 q0 =
      case mulLo128 q1 q0 d1 d0 of { (# p1, p0 #) ->
      case sub128 n1 n0 p1 p0 of { (# r1, r0 #) ->
      (Word128 q1 q0, Word128 r1 r0) }}

-- | Type-level numerators.
type KnownNumerator :: Nat -> Type -> Constraint
class KnownNumerator n a where
  -- | The numerator as a value.
  numeratorVal :: a

instance (CheckNumerator 128 n, KnownNat (Div n (2 ^ 64)), KnownNat (Mod n (2 ^ 64))) => KnownNumerator n Word128 where
  {-# INLINE numeratorVal #-}
  numeratorVal = Word128 (natWord64 @(Div n (2 ^ 64))) (natWord64 @(Mod n (2 ^ 64)))

instance (CheckNumerator 64 n, KnownNat n) => KnownNumerator n Word64 where
  {-# INLINE numeratorVal #-}
  numeratorVal = natWord64 @n

instance (CheckNumerator 32 n, KnownNat n) => KnownNumerator n Word32 where
  {-# INLINE numeratorVal #-}
  numeratorVal = fromIntegral (natWord64 @n)

instance (CheckNumerator 16 n, KnownNat n) => KnownNumerator n Word16 where
  {-# INLINE numeratorVal #-}
  numeratorVal = fromIntegral (natWord64 @n)

instance (CheckNumerator 8 n, KnownNat n) => KnownNumerator n Word8 where
  {-# INLINE numeratorVal #-}
  numeratorVal = fromIntegral (natWord64 @n)

-- | @divRemN \@n d == divRem n d@ for a type-level numerator. See 'divRemConst'.
{-# INLINE divRemN #-}
divRemN :: forall n a. (KnownNumerator n a, StrengthReduce a) => StrengthReduced a -> (a, a)
divRemN = divRemConst (numeratorVal @n)

-- | Quotient of the type-level numerator @n@.
{-# INLINE divN #-}
divN :: forall n a. (KnownNumerator n a, StrengthReduce a) => StrengthReduced a -> a
divN d = case divRemN @n d of (q, _) -> q

-- | Remainder of the type-level numerator @n@.
{-# INLINE remN #-}
remN :: forall n a. (KnownNumerator n a, StrengthReduce a) => StrengthReduced a -> a
remN d = case divRemN @n d of (_, r) -> r

-- | @divRemNonZeroN \@n d == divRemNonZero n d@ for a type-level numerator.
-- See 'divRemNonZeroConst'.
{-# INLINE divRemNonZeroN #-}
divRemNonZeroN :: forall n a. (KnownNumerator n a, StrengthReduce a) => NonZero a -> (a, a)
divRemNonZeroN = divRemNonZeroConst (numeratorVal @n)

-- | Unchecked hardware quotient of the type-level numerator @n@.
{-# INLINE divNonZeroN #-}
divNonZeroN :: forall n a. (KnownNumerator n a, StrengthReduce a) => NonZero a -> a
divNonZeroN d = case divRemNonZeroN @n d of (q, _) -> q

-- | Unchecked hardware remainder of the type-level numerator @n@.
{-# INLINE remNonZeroN #-}
remNonZeroN :: forall n a. (KnownNumerator n a, StrengthReduce a) => NonZero a -> a
remNonZeroN d = case divRemNonZeroN @n d of (_, r) -> r

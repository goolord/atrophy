# Revision history for atrophy

## 0.2.0.0 -- 2026-09-15

A rewrite for performance. The API has changed.

* Division is now built on GHC's double-word primops (`timesWord2#`,
  `quotRemWord2#`) instead of boxed `Word128` arithmetic, and never allocates.
* `Word64` uses Granlund-Montgomery: one multiplication, no branches, no
  special cases for 1 or powers of two.
* `Word32`, `Word16` and `Word8` use a 64-bit reciprocal (Lemire et al.): one
  multiplication.
* `Word128` strength reduction is now implemented. `new` for `Word128` uses
  hardware 128/64 divisions and a 3-by-2 normalized division rather than
  `Integer`.
* New `StrengthReduce` class with an injective `StrengthReduced` type family
  replaces the `HasField`-based functions. `new`, `divRem`, `div'` and `rem'`
  work at every width; `new64`, `div64`, `rem64` and `divRem64` are gone.
  The `StrengthReducedW*` types are now abstract.
* Numerators known at compile time: `divRemConst` and friends (value level, for
  literals) and `divRemN @n` (type level). Zero, one, powers of two and
  `maxBound` avoid the multiplication entirely; `Word128` numerators below
  2^64 need two multiplications instead of eight.
* `divRemNonZero` and friends: hardware division without the zero check.
  `divRemNonZeroN @n` uses a 32-bit division when the numerator fits.
* Divisors known at compile time: `divRemK @d`, `divK @d`, `remK @d`. GHC's
  native code generator does not strength-reduce division by constants; these
  compute libdivide's magic numbers during type checking.
* `Atrophy.LongDivision` now uses Möller-Granlund 2-by-1 division with a
  precomputed reciprocal (`Divisor2By1`), takes little-endian limbs, returns the
  remainder, and works for every divisor. The previous implementation processed
  limbs most significant first and was incorrect for divisors of 2^32 or more.
  `divide128By64Preshifted` and `divide128MaxBy64` have been removed.
* `Atrophy.LongMultiplication` works on `primitive`'s `PrimArray` and
  `MutablePrimArray` directly. `multiply256By128UpperBits` is a straight chain of
  64-bit multiplications.
* Dropped the `contiguous` dependency. Now depends on `primitive` and
  `deepseq`; `NFData` instances are provided.
* `NonZero` gains `Eq`, `Ord` and `NFData` instances, `nonZero` and
  `getNonZero`.
* Requires GHC 9.4 or later.

## 0.1.0.0 -- 2022

* First version. Released on an unsuspecting world.

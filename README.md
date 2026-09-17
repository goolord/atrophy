# Atrophy
Fast div/mod via arithmetic strength reduction.

Precompute a divisor once, then divide by it with a multiplication and a few
shifts instead of a hardware division.

```haskell
import Atrophy

let d = new (NonZero 7) :: StrengthReduced Word64
div' 100 d  -- 14
rem' 100 d  -- 2
```

Works for `Word8`, `Word16`, `Word32`, `Word64` and `Word128` through the
`StrengthReduce` class. Everything is `INLINE`, branchless where it matters, and
never allocates.

## Constants

GHC's native code generator does **not** strength-reduce division by constants:
``x `quot` 7`` compiles to a `div` instruction. `Atrophy.Known` computes the
magic numbers during type checking instead:

```haskell
divK @7 x       -- one multiplication, no division
remK @1000 x
```

Numerators can also be known at compile time, with a runtime divisor:

```haskell
divN @(2 ^ 63) d          -- d :: StrengthReduced Word64; no multiplication at all
divNonZeroN @1000000 d    -- d :: NonZero Word64; a 32-bit hardware division
```

Zero, one, powers of two and `maxBound` skip the multiplication entirely, and
`Word128` numerators below 2^64 need two 64-bit multiplications instead of
eight. `divConst` and friends do the same for literal numerators passed as
values; GHC folds the branches away.

## Algorithms

* `Word64`: Granlund & Montgomery, "Division by Invariant Integers using
  Multiplication". One `mul`, no branches, no special cases for 1 or powers of
  two.
* `Word32` and smaller: Lemire, Kaser & Kurz, "Faster Remainder by Direct
  Computation". One `mul`.
* `Word128`: Granlund & Montgomery on 64-bit limbs. `new` uses hardware 128/64
  divisions and a normalized 3-by-2 division.
* Compile-time divisors: libdivide's unsigned algorithm, choosing between shift,
  multiply-shift and multiply-add-shift at compile time.
* `Atrophy.LongDivision`: Möller & Granlund, "Improved division by invariant
  integers", for little-endian multi-limb numbers divided by a 64-bit divisor.

## Benchmarks

Time per operation, averaged over 10000 uniformly random dividends.
Divisors have a uniformly random bit length. GHC 9.14.1, native code generator,
AMD Ryzen 7 7840U. Run them yourself with `cabal bench`.

One divisor, many dividends; `new` is paid once, then amortized away:

| Type      | GHC `quot` |   `new` |  `div'` |  `rem'` |
|-----------|-----------:|--------:|--------:|--------:|
| `Word32`  |    1.29 ns | 1.88 ns | 0.70 ns |     n/a |
| `Word64`  |    1.52 ns | 2.92 ns | 0.92 ns | 1.13 ns |
| `Word128` |     121 ns | 10.8 ns | 4.89 ns |     n/a |

A fresh divisor for every dividend, so `new` is paid every time:

| Type      | GHC `quot` | `new` + `div'` |
|-----------|-----------:|---------------:|
| `Word32`  |    1.29 ns |        1.72 ns |
| `Word64`  |    1.61 ns |        2.54 ns |
| `Word128` |    39.2 ns |        16.3 ns |

`Word128` `quot` comes from `wide-word`; `divNonZero`, an unchecked hardware
division, takes 4.12 ns.

Compile-time constants:

| Constant                                 | GHC `quot` | atrophy |
|------------------------------------------|-----------:|--------:|
| `Word32` divisor 7, `divK`               |    1.29 ns | 0.69 ns |
| `Word64` divisor 7, `divK`               |    1.91 ns | 0.85 ns |
| `Word64` divisor 10^9+7, `divK`          |    1.50 ns | 0.88 ns |
| `Word128` divisor 10^19, `divK`          |    4.68 ns | 3.91 ns |
| `Word64` numerator 10^6, `divNonZeroN`   |    1.50 ns | 0.52 ns |
| `Word64` numerator 2^63, `divNonZeroN`   |    3.05 ns | 1.61 ns |
| `Word128` numerator 10^18, `divNonZeroN` |    5.13 ns | 0.92 ns |

A constant numerator also helps with a precomputed divisor, here cycling through
64 of them:

| Numerator          |  `div'` |  `divN` |
|--------------------|--------:|--------:|
| `Word64`, 2^63     | 2.26 ns | 1.78 ns |
| `Word64`, 2^64 - 1 | 2.23 ns | 1.78 ns |
| `Word128`, 10^18   | 5.40 ns | 2.80 ns |

Dividing a 64-limb number by a 64-bit divisor with `longDivision` takes 281 ns,
including allocating the quotient; GMP's hand-written assembly, via `Integer`,
takes 166 ns.

Performance is *heavily* platform dependent. Zen 4 has an unusually fast
hardware divider, so these numbers understate the gains on most other CPUs,
where a 64-bit `div` costs 35 to 90 cycles rather than 10 to 20. Even here, for
`Word32` and `Word64`, a hardware division beats `new` followed by a single
`div'`, so strength reduction pays off once a divisor is reused. `Word128` wins
either way.

## Special thanks
Originally based on https://github.com/ejmahler/strength_reduce

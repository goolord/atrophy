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

Works for `Word8`, `Word16`, `Word32`, `Word64` and `Word128`, all through the
`StrengthReduce` class. Everything is `INLINE`, branchless where it matters, and
never allocates.

## Constants

GHC's native code generator does **not** strength-reduce division by constants:
``x `quot` 7`` compiles to a `div` instruction. `Atrophy.Known` does it for you,
computing the magic numbers during type checking:

```haskell
divK @7 x       -- one multiplication, no division
remK @1000 x
```

It also handles numerators known at compile time, with a runtime divisor:

```haskell
divN @(2 ^ 63) d          -- d :: StrengthReduced Word64; no multiplication at all
divNonZeroN @1000000 d    -- d :: NonZero Word64; a 32-bit hardware division
```

Zero, one, powers of two and `maxBound` skip the multiplication entirely, and
`Word128` numerators below 2^64 need two 64-bit multiplications instead of eight.
`divConst` and friends do the same for literal numerators passed as values; GHC
folds the branches away.

## Algorithms

* `Word64`: Granlund & Montgomery, "Division by Invariant Integers using
  Multiplication". One `mul`, no branches, no special cases for 1 or powers of
  two.
* `Word32` and smaller: Lemire, Kaser & Kurz, "Faster Remainder by Direct
  Computation". One `mul`.
* `Word128`: Granlund & Montgomery on 64-bit limbs. `new` uses hardware 128/64
  divisions and a normalized 3-by-2 division.
* Compile-time divisors: libdivide's unsigned algorithm, with the choice between
  shift, multiply-shift and multiply-add-shift made at compile time.
* `Atrophy.LongDivision`: Möller & Granlund, "Improved division by invariant
  integers", for dividing little-endian multi-limb numbers by a 64-bit divisor.

## Benchmarks

Nanoseconds per operation, averaged over 10000 uniformly random dividends.
Divisors have a uniformly random bit length. GHC 9.14.1, native code generator,
AMD Ryzen 7 7840U. "atrophy 0.1" is the previous release, run on the same data.

| `Word64`                           | GHC `quot` | atrophy 0.1 | atrophy |
|------------------------------------|-----------:|------------:|--------:|
| `new`                              |            |        5.56 |    2.92 |
| one divisor, many dividends        |       1.52 |        17.6 |    0.92 |
| one divisor, remainder             |            |             |    1.13 |
| unique divisors (`new` + `div'`)   |       1.61 |        33.5 |    2.54 |
| constant divisor 7 (`divK`)        |       1.91 |             |    0.85 |
| constant divisor 10^9+7 (`divK`)   |       1.50 |             |    0.88 |
| constant numerator 10^6, `NonZero` divisor (`divNonZeroN`) | 1.50 | | 0.52 |
| constant numerator 2^63, `NonZero` divisor (`divNonZeroN`) | 3.05 | | 1.61 |

| `Word32`                           | GHC `quot` | atrophy 0.1 | atrophy |
|------------------------------------|-----------:|------------:|--------:|
| `new`                              |            |        1.56 |    1.88 |
| one divisor, many dividends        |       1.29 |        1.05 |    0.70 |
| unique divisors (`new` + `div'`)   |       1.29 |        1.61 |    1.72 |
| constant divisor 7 (`divK`)        |       1.29 |             |    0.69 |

| `Word128`                          | wide-word `quot` | atrophy |
|------------------------------------|-----------------:|--------:|
| `new`                              |                  |    10.8 |
| one divisor, many dividends        |              121 |    4.89 |
| unchecked hardware division (`divNonZero`) |          |    4.12 |
| unique divisors (`new` + `div'`)   |             39.2 |    16.3 |
| constant divisor 10^19 (`divK`)    |             4.68 |    3.91 |
| constant numerator 10^18, `NonZero` divisor (`divNonZeroN`) | 5.13 | 0.92 |

Constant numerators with a precomputed divisor, cycling through 64 divisors:

|                                    | `div'` | `divN` |
|------------------------------------|-------:|-------:|
| `Word64`, numerator 2^63           |   2.26 |   1.78 |
| `Word64`, numerator 2^64 - 1       |   2.23 |   1.78 |
| `Word128`, numerator 10^18         |   5.40 |   2.80 |

Dividing a 64-limb number by a 64-bit divisor with `longDivision` takes 281 ns,
including allocating the quotient; GMP's hand-written assembly, via `Integer`,
takes 166 ns.

Performance is *heavily* platform dependent. Zen 4 has an unusually fast
hardware divider, so these numbers understate the gains on most other CPUs,
where a 64-bit `div` costs 35 to 90 cycles rather than 10 to 20. On this machine
a hardware division is still faster than `new` followed by a single `div'`, so
strength reduction pays off when a divisor is reused.

Run them yourself with `cabal bench`.

## Special thanks
Originally based on https://github.com/ejmahler/strength_reduce

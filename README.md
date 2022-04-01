# Atrophy
Fast div/mod via arithmetic strength reduction.

Good compilers already perform this optimization for divisors that are known at compile time; this library enables this optimization for divisors that are only known at runtime.

## Benchmarks

```
All
  atrophy
    Word64
      new:                  OK (1.38s)
        40.9 ns ± 330 ps
      div 10000 uniques:    OK (3.82s)
        929  μs ± 5.7 μs
      div 10000, 1 divisor: OK (1.81s)
        219  μs ± 3.2 μs
    Word32
      new:                  OK (0.77s)
        11.4 ns ± 162 ps
      div 10000 uniques:    OK (0.89s)
        433  μs ± 6.4 μs
      div 10000, 1 divisor: OK (13.59s)
        208  μs ± 1.2 μs
  ghc
    Word64
      div 10000 uniques:    OK (2.12s)
        253  μs ± 2.8 μs
      div 10000, 1 divisor: OK (2.05s)
        248  μs ± 2.8 μs
    Word32
      div 10000 uniques:    OK (2.15s)
        255  μs ± 3.4 μs
      div 10000, 1 divisor: OK (2.12s)
        255  μs ± 2.6 μs
```

note: performance is *heavily* platform dependant.

there is a *lot* of room for improvement, but atrophy does outperform `div` in the best case.
the rust library this is based off of has much more impressive benchmarks, though I suspect llvm is doing a lot of heavy lifting

## Special thanks
Code based on https://github.com/ejmahler/strength_reduce

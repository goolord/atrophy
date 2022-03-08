# Atrophy
Fast div/mod via arithmetic strength reduction.

Good compilers already perform this optimization for divisors that are known at compile time; this library enables this optimization for divisors that are only known at runtime.

## Benchmarks

```
All
  atrophy
    Word64
      new64:                OK (0.30s)
        67.4 ns ± 2.9 ns
      div 10000 uniques:    OK (0.18s)
        1.38 ms ± 119 μs
      div 10000, 1 divisor: OK (0.46s)
        216  μs ± 6.8 μs
    Word32
      new:                  OK (0.17s)
        10.2 ns ± 652 ps
      div 10000 uniques:    OK (0.60s)
        275  μs ± 8.4 μs
      div 10000, 1 divisor: OK (0.21s)
        209  μs ±  20 μs
  ghc
    Word64
      div 10000 uniques:    OK (0.58s)
        265  μs ±  14 μs
      div 10000, 1 divisor: OK (0.53s)
        247  μs ± 6.8 μs
    Word32
      div 10000 uniques:    OK (0.57s)
        263  μs ± 5.9 μs
      div 10000, 1 divisor: OK (0.53s)
        251  μs ±  21 μs
```

there is a *lot* of room for improvement, but atrophy does outperform `div` in the best case.
the rust library this is based off of has much more impressive benchmarks, though I suspect llvm is doing a lot of heavy lifting

## Special thanks
Code based on https://github.com/ejmahler/strength_reduce

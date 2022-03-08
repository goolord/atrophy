{-# LANGUAGE
    BangPatterns
  , GADTs
  , DeriveGeneric
  , StandaloneDeriving
  , MagicHash
  , DataKinds
  , GeneralizedNewtypeDeriving
  , TypeApplications
  , ScopedTypeVariables
  , NumericUnderscores
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

module Main where

import Test.Tasty.Bench (bench, bgroup, defaultMain, nf, Benchmark, envWithCleanup, nfIO)
import Control.DeepSeq (NFData, force)
import Atrophy
import GHC.Generics
import Data.Word (Word64, Word32)
import Control.Exception (evaluate)
import Test.Tasty (withResource)
import System.Mem (performMajorGC)
import System.Random.Stateful
import Data.Proxy (Proxy (Proxy))

deriving instance Generic StrengthReducedW64
instance NFData StrengthReducedW64

deriving instance Generic StrengthReducedW32
instance NFData StrengthReducedW32

manyRandom :: forall a. (Uniform a, NFData a) => IO [a]
manyRandom = uniformListM 10_000 globalStdGen

randomEnv :: NFData b => IO b
  -> (b -> Benchmark)
  -> Benchmark
randomEnv a = envWithCleanup (a >>= evaluate . force) (const performMajorGC)

randomEnv' :: NFData b => IO b
  -> (IO b -> Benchmark)
  -> Benchmark
randomEnv' a = withResource (a >>= evaluate . force) (const performMajorGC)

main :: IO ()
main = do
  defaultMain $
    [ bgroup "atrophy"
        [ bgroup "Word64"
          [ randomEnv (uniformM globalStdGen) $ \divisor' ->
              bench "new64" $ nf new64 divisor'
          , randomEnv (manyRandom @(Word64, Word64)) $ \somePairs ->
              bench "div 10000 uniques" $ nf (fmap (\(x, y) -> x `div64` new64 y)) somePairs
          , randomEnv' ((,) <$> (new64 <$> randomRIO (1, maxBound)) <*> manyRandom @Word64) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
              (divisor', dividends) <- x
              pure $ fmap (\dividend' -> dividend' `div64` divisor') dividends
          ]
        , bgroup "Word32"
          [ randomEnv (uniformM globalStdGen) $ \divisor' ->
              bench "new" $ nf (new StrengthReducedW32) divisor'
          , randomEnv (manyRandom @(Word32, Word32)) $ \somePairs ->
              bench "div 10000 uniques" $ nf (fmap (\(x, y) -> x `div'` new StrengthReducedW32 y)) somePairs
          , randomEnv' ((,) <$> (new StrengthReducedW32 <$> randomRIO (1, maxBound)) <*> manyRandom @Word32) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
              (divisor', dividends) <- x
              pure $ fmap (\dividend' -> dividend' `div'` divisor') dividends
          ]
        ]
    , bgroup "ghc"
        [ bgroup "Word64" $ ghcBench (Proxy @Word64)
        , bgroup "Word32" $ ghcBench (Proxy @Word32)
        ]
    ]

ghcBench :: forall a. (NFData a, Uniform a, Integral a, Random a, Bounded a) => Proxy a -> [Benchmark]
ghcBench _ =
  [ randomEnv (manyRandom @(a, a)) $ \somePairs ->
      bench "div 10000 uniques" $ nf (\xs -> fmap (\(x, y) -> x `div` y) xs) somePairs
  , randomEnv' ((,) <$> randomRIO (1, maxBound) <*> manyRandom @a) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
      (divisor', dividends) <- x
      pure $ fmap (\dividend' -> dividend' `div` divisor') dividends
  ]

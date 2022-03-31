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
{-# LANGUAGE DerivingStrategies #-}

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

instance (Bounded a, Num a, UniformRange a) => Uniform (NonZero a) where
  uniformM g = NonZero <$> uniformRM (1, maxBound) g

deriving newtype instance NFData a => NFData (NonZero a)

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
        [ bgroup "Word64" $ atrophyBench div64 new64
        , bgroup "Word32" $ atrophyBench div' (new StrengthReducedW32)
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

atrophyBench :: forall base sr out. (NFData base, Uniform base, UniformRange base, NFData sr, NFData out, Random base, Num base, Bounded base) => (base -> sr -> out) -> (NonZero base -> sr) -> [Benchmark]
atrophyBench divF newF =
  [ randomEnv (uniformM globalStdGen) $ \divisor' ->
      bench "new" $ nf newF divisor'
  , randomEnv (manyRandom @(base, base)) $ \somePairs ->
      bench "div 10000 uniques" $ nf (fmap (\(x, y) -> x `divF` newF (NonZero y))) somePairs
  , randomEnv' ((,) <$> (newF <$> (NonZero <$> randomRIO (1, maxBound))) <*> manyRandom @base) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
      (divisor', dividends) <- x
      pure $ fmap (\dividend' -> dividend' `divF` divisor') dividends
  ]

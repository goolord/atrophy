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
import Data.Word (Word64)
import Control.Exception (evaluate)
import Test.Tasty (withResource)
import System.Mem (performMajorGC)
import System.Random.Stateful

deriving instance Generic StrengthReducedW64

instance NFData StrengthReducedW64

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
        [ randomEnv (uniformM globalStdGen) $ \divisor' ->
            bench "new" $ nf (new StrengthReducedW64) divisor'
        , randomEnv (manyRandom @(Word64, Word64)) $ \somePairs ->
            bench "div 10000 uniques" $ nf (fmap (\(x, y) -> x `div'` new StrengthReducedW64 y)) somePairs
        , randomEnv' ((,) <$> (new StrengthReducedW64 <$> randomRIO (1, maxBound)) <*> manyRandom @Word64) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
            (divisor', dividends) <- x
            pure $ fmap (\dividend' -> dividend' `div'` divisor') dividends
        ]
    , bgroup "ghc"
        [ randomEnv (manyRandom @(Word64, Word64)) $ \somePairs ->
            bench "div 10000 uniques" $ nf (\xs -> fmap (\(x, y) -> x `div` y) xs) somePairs
        , randomEnv' ((,) <$> randomRIO (1, maxBound) <*> manyRandom @Word64) $ \x -> bench "div 10000, 1 divisor" $ nfIO $ do 
            (divisor', dividends) <- x
            pure $ fmap (\dividend' -> dividend' `div` divisor') dividends
        ]
    ]

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Main (main) where

import Atrophy
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Control.Monad.ST (runST)
import Data.Bits
import Data.Primitive.PrimArray
import Data.Primitive.SmallArray
import Data.WideWord.Word128
import Data.Primitive.Types (Prim)
import Data.Word
import GHC.TypeNats (type (-), type (^))
import System.Random.Stateful
import Test.Tasty.Bench

size :: Int
size = 10000

-- | Dividends, divisors (never zero) and one fixed divisor.
data Env a = Env !(PrimArray a) !(PrimArray a) !a

instance NFData (Env a) where
  rnf !_ = ()

-- | Precomputed divisors, for the known-numerator benchmarks.
newtype Reduced a = Reduced (SmallArray (StrengthReduced a))

instance NFData (Reduced a) where
  rnf (Reduced !_) = ()

class (Prim a, Integral a, FiniteBits a, Bounded a, StrengthReduce a) => BenchWord a where
  uniformW :: StdGen -> (a, StdGen)

instance BenchWord Word32 where uniformW = uniform
instance BenchWord Word64 where uniformW = uniform
instance BenchWord Word128 where
  uniformW g0 = let (h, g1) = uniform g0; (l, g2) = uniform g1 in (Word128 h l, g2)

-- | Dividends are uniform. Divisors have a uniform bit length, then are uniform
-- within it: this exercises both small and large quotients, which matters for
-- hardware division latency.
mkEnv :: forall a. BenchWord a => IO (Env a)
mkEnv = do
  let g0 = mkStdGen 2026
      (ns, g1) = go size g0 []
      (ds, g2) = goD size g1 []
      (d, _) = goD 1 g2 []
  pure $ Env (primArrayFromList ns) (primArrayFromList ds) (sum d)
  where
  go 0 g acc = (acc, g)
  go k g acc = let (x, g') = uniformW g in go (k - 1 :: Int) g' (x : acc)
  goD 0 g acc = (acc, g)
  goD k g acc =
    let (bitsLen, g') = uniformR (1, finiteBitSize (0 :: a)) g
        (x, g'') = uniformW g'
    in goD (k - 1 :: Int) g'' (max 1 (x `shiftR` (finiteBitSize (0 :: a) - bitsLen)) : acc)

-- | A small table, so that the benchmark measures division rather than cache
-- misses on 10000 boxed values.
tableSize :: Int
tableSize = 64

mkReduced :: BenchWord a => Env a -> Reduced a
mkReduced (Env _ ds _) = Reduced $ runST $ do
  arr <- newSmallArray tableSize undefined
  let go i | i == tableSize = pure ()
           | otherwise = (writeSmallArray arr i $! new (NonZero (indexPrimArray ds i))) >> go (i + 1)
  go 0
  unsafeFreezeSmallArray arr

{-# INLINE sumMap #-}
sumMap :: (Prim a, Num a) => (a -> a) -> PrimArray a -> a
sumMap f = foldlPrimArray' (\acc x -> acc + f x) 0

{-# INLINE sumIx #-}
sumIx :: Num a => (Int -> a) -> a
sumIx f = go 0 0
  where
  go !i !acc
    | i == size = acc
    | otherwise = go (i + 1) (acc + f i)

{-# INLINE sumReduced #-}
sumReduced :: Num a => (StrengthReduced a -> a) -> Reduced a -> a
sumReduced f (Reduced arr) = sumIx (\i -> f (indexSmallArray arr (i .&. (tableSize - 1))))

{-# NOINLINE runtime #-}
runtime :: a -> a
runtime x = x

main :: IO ()
main = defaultMain
  [ bgroup "Word64" $ runtimeBenches @Word64 ++
      [ env (mkEnv @Word64) $ \ ~(Env ns _ _) -> bgroup "known divisor 7"
          [ bench "ghc quot" $ whnf (sumMap (`quot` 7)) ns
          , bench "atrophy divK" $ whnf (sumMap (divK @7)) ns
          , bench "atrophy div' (runtime)" $ whnf (\sr -> sumMap (`div'` sr) ns) (new (NonZero (runtime 7)))
          ]
      , env (mkEnv @Word64) $ \ ~(Env ns _ _) -> bgroup "known divisor 1000000007"
          [ bench "ghc quot" $ whnf (sumMap (`quot` 1000000007)) ns
          , bench "atrophy divK" $ whnf (sumMap (divK @1000000007)) ns
          ]
      , env (mkEnv @Word64) $ \ ~(e@(Env _ ds _)) -> env (pure (mkReduced e)) $ \rs -> bgroup "known numerator 2^63"
          [ bench "ghc quot" $ whnf (sumMap (quot (2 ^ (63 :: Int)))) ds
          , bench "atrophy divNonZeroN" $ whnf (sumMap (divNonZeroN @(2 ^ 63) . NonZero)) ds
          , bench "atrophy div' (runtime)" $ whnf (\n -> sumReduced (div' n) rs) (runtime (2 ^ (63 :: Int)))
          , bench "atrophy divN" $ whnf (sumReduced (divN @(2 ^ 63))) rs
          ]
      , env (mkEnv @Word64) $ \ ~(e@(Env _ ds _)) -> env (pure (mkReduced e)) $ \rs -> bgroup "known numerator 1000000"
          [ bench "ghc quot" $ whnf (sumMap (quot 1000000)) ds
          , bench "atrophy divNonZeroN" $ whnf (sumMap (divNonZeroN @1000000 . NonZero)) ds
          , bench "atrophy div' (runtime)" $ whnf (\n -> sumReduced (div' n) rs) (runtime 1000000)
          , bench "atrophy divN" $ whnf (sumReduced (divN @1000000)) rs
          ]
      , env (mkEnv @Word64) $ \ ~(e@(Env _ ds _)) -> env (pure (mkReduced e)) $ \rs -> bgroup "known numerator maxBound"
          [ bench "ghc quot" $ whnf (sumMap (quot maxBound)) ds
          , bench "atrophy div' (runtime)" $ whnf (\n -> sumReduced (div' n) rs) (runtime maxBound)
          , bench "atrophy divN" $ whnf (sumReduced (divN @(2 ^ 64 - 1))) rs
          ]
      , env mkLimbs $ \ ~(ls, d) -> bgroup "long division 64 limbs"
          [ bench "Integer quotRem" $ whnf (\n -> n `quotRem` fromIntegral d) (limbsToInteger ls)
          , bench "atrophy longDivision" $ whnf (\dv -> runST $ do
              q <- newPrimArray 64
              longDivision dv ls q) (newDivisor2By1 (NonZero d))
          ]
      ]
  , bgroup "Word32" $ runtimeBenches @Word32 ++
      [ env (mkEnv @Word32) $ \ ~(Env ns _ _) -> bgroup "known divisor 7"
          [ bench "ghc quot" $ whnf (sumMap (`quot` 7)) ns
          , bench "atrophy divK" $ whnf (sumMap (divK @7)) ns
          ]
      ]
  , bgroup "Word128" $ runtimeBenches @Word128 ++
      [ env (mkEnv @Word128) $ \ ~(Env ns _ _) -> bgroup "known divisor 10^19"
          [ bench "wide-word quot" $ whnf (sumMap (`quot` 10000000000000000000)) ns
          , bench "atrophy divK" $ whnf (sumMap (divK @10000000000000000000)) ns
          ]
      , env (mkEnv @Word128) $ \ ~(e@(Env _ ds _)) -> env (pure (mkReduced e)) $ \rs -> bgroup "known numerator 10^18"
          [ bench "wide-word quot" $ whnf (sumMap (quot 1000000000000000000)) ds
          , bench "atrophy divNonZeroN" $ whnf (sumMap (divNonZeroN @1000000000000000000 . NonZero)) ds
          , bench "atrophy div' (runtime)" $ whnf (\n -> sumReduced (div' n) rs) (runtime 1000000000000000000)
          , bench "atrophy divN" $ whnf (sumReduced (divN @1000000000000000000)) rs
          ]
      ]
  ]

runtimeBenches :: forall a. BenchWord a => [Benchmark]
runtimeBenches =
  [ env (mkEnv @a) $ \ ~(Env _ _ d) -> bench "new" $ whnf new (NonZero d)
  , env (mkEnv @a) $ \ ~(Env ns _ d) -> bgroup "fixed divisor"
      [ bench "baseline quot" $ whnf (\dv -> sumMap (`quot` dv) ns) d
      , bench "atrophy divNonZero" $ whnf (\dv -> sumMap (`divNonZero` dv) ns) (NonZero d)
      , bench "atrophy div'" $ whnf (\sr -> sumMap (`div'` sr) ns) (new (NonZero d))
      , bench "atrophy rem'" $ whnf (\sr -> sumMap (`rem'` sr) ns) (new (NonZero d))
      ]
  , env (mkEnv @a) $ \ ~(Env ns ds _) -> bgroup "unique divisors"
      [ bench "baseline quot" $ whnf (\xs -> sumIx (\i -> indexPrimArray xs i `quot` indexPrimArray ds i)) ns
      , bench "atrophy div' . new" $ whnf (\xs -> sumIx (\i -> indexPrimArray xs i `div'` new (NonZero (indexPrimArray ds i)))) ns
      ]
  ]

mkLimbs :: IO (PrimArray Word64, Word64)
mkLimbs = do
  g <- newIOGenM (mkStdGen 7)
  ls <- replicateM 64 (uniformM g)
  d <- uniformRM (1, maxBound) g
  pure (primArrayFromList ls, d)

limbsToInteger :: PrimArray Word64 -> Integer
limbsToInteger = foldrPrimArray (\l acc -> acc * 2 ^ (64 :: Int) + toInteger l) 0

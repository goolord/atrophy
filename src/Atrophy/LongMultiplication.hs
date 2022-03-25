{-# LANGUAGE
    TypeApplications
  , ScopedTypeVariables
  , LambdaCase
#-}

module Atrophy.LongMultiplication where

import Data.WideWord.Word128
import Data.Word
import qualified Data.Primitive.Contiguous as Contiguous
import Data.Primitive.Contiguous (PrimArray, MutableSliced, Mutable)
import Control.Monad.ST.Strict (ST)
import Data.STRef.Strict (newSTRef, modifySTRef, readSTRef)
import Data.Bits
import Data.Foldable (for_)

{-# INLINE multiply256By128UpperBits #-}
multiply256By128UpperBits :: Word128 -> Word128 -> Word128 -> Word128
multiply256By128UpperBits aHi aLo b =
  let
    -- Break a and b into little-endian 64-bit chunks
    aChunks :: PrimArray Word64
    aChunks = Contiguous.quadrupleton
      (word128Lo64 aLo)
      (word128Hi64 aLo)
      (word128Lo64 aHi)
      (word128Hi64 aHi)
    bChunks :: PrimArray Word64
    bChunks = Contiguous.doubleton
      (word128Lo64 b)
      (word128Hi64 b)

    -- Multiply b by a, one chunk of b at a time
    prod :: PrimArray Word64
    prod = Contiguous.create $ do
      prod' <- Contiguous.replicateMut 6 0
      flip Contiguous.itraverse_ bChunks $ \bIndex bDigit -> do
        pSize <- Contiguous.sizeMut prod'
        multiply256By64Helper
            (Contiguous.sliceMut prod' bIndex (pSize - bIndex))
            aChunks
            bDigit
      pure prod'

  in Word128
    { word128Hi64 = Contiguous.index prod 5
    , word128Lo64 = Contiguous.index prod 4
    }

{-# INLINE multiply256By64Helper #-}
multiply256By64Helper :: forall s. MutableSliced PrimArray s Word64 -> PrimArray Word64 -> Word64 -> ST s ()
multiply256By64Helper _ _ 0 = pure ()
multiply256By64Helper prod a b = do
  carry <- newSTRef 0
  productSize <- Contiguous.sizeMut prod
  let
    aSize = Contiguous.size a
    productLo :: MutableSliced PrimArray s Word64
    productLo = Contiguous.sliceMut prod 0 aSize
    productHi :: MutableSliced PrimArray s Word64
    productHi = Contiguous.sliceMut prod aSize (productSize - aSize)
  -- Multiply each of the digits in a by b, adding them into the 'prod' value.
  -- We don't zero out prod, because we this will be called multiple times, so it probably contains a previous iteration's partial prod, and we're adding + carrying on top of it
  for_ [0..aSize - 1] $ \i -> do
    p <- Contiguous.read productLo i
    let aDigit = Contiguous.index a i
    modifySTRef carry $ \x -> x
      + Word128 0 p
      + (Word128 0 aDigit * Word128 0 b)
    Contiguous.write prod i . word128Lo64 =<< readSTRef carry
    modifySTRef carry (`unsafeShiftR` 64)

  let productHiSize = productSize - aSize
  for_ [0..productHiSize - 1] $ \i -> do
    p <- Contiguous.read productHi i
    modifySTRef carry (+ Word128 0 p)
    Contiguous.write productHi i . word128Lo64 =<< readSTRef carry
    modifySTRef carry (`unsafeShiftR` 64)

  readSTRef carry >>= \case
    0 -> pure ()
    _ -> error "carry overflow during multiplication!"

-- compute prod += a * b
{-# INLINE longMultiply #-}
longMultiply :: forall s. PrimArray Word64 -> Word64 -> Mutable PrimArray s Word64 -> ST s ()
longMultiply a b prod = do
  prod' <- Contiguous.toSliceMut prod
  multiply256By64Helper prod' a b

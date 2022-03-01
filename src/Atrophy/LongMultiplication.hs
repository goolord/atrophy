{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Atrophy.LongMultiplication where

import Data.WideWord.Word128
import Data.Word
import qualified Data.Primitive.Contiguous as Contiguous
import Data.Primitive.Contiguous (SmallArray, Slice, Mutable, MutableSliced)
import Control.Monad.ST.Strict (ST)
import Data.STRef.Strict (newSTRef, modifySTRef, readSTRef)
import Data.Bits
import Data.Foldable (for_)
import GHC.Stack (HasCallStack)

{-# INLINE multiply256By128UpperBits #-}
multiply256By128UpperBits :: HasCallStack => Word128 -> Word128 -> Word128 -> Word128
multiply256By128UpperBits aHi aLo b =
  let 
    -- Break a and b into little-endian 64-bit chunks
    aChunks :: SmallArray Word64
    aChunks = Contiguous.quadrupleton
      (word128Lo64 aLo)
      (word128Hi64 aLo)
      (word128Lo64 aHi)
      (word128Hi64 aHi)
    bChunks :: SmallArray Word64
    bChunks = Contiguous.doubleton
      (word128Lo64 b)
      (word128Hi64 b)

    -- Multiply b by a, one chunk of b at a time
    product :: SmallArray Word64
    product = Contiguous.create $ do
      product <- Contiguous.replicateMut 6 0
      flip Contiguous.itraverse_ bChunks $ \bIndex bDigit -> do
        pSize <- Contiguous.sizeMut product
        multiply256By64Helper 
            (Contiguous.sliceMut product bIndex (pSize - bIndex))
            aChunks
            bDigit
      pure product

  in Word128 
    { word128Hi64 = Contiguous.index product 5
    , word128Lo64 = Contiguous.index product 4
    }

-- compute product += a * b
{-# INLINE multiply256By64Helper #-}
multiply256By64Helper :: forall s. HasCallStack => MutableSliced SmallArray s Word64 -> SmallArray Word64 -> Word64 -> ST s ()
multiply256By64Helper product _ 0 = pure ()
multiply256By64Helper product a b = do
  carry <- newSTRef 0
  productSize <- Contiguous.sizeMut product
  let 
    aSize = Contiguous.size a
    productLo :: MutableSliced SmallArray s Word64
    productLo = Contiguous.sliceMut product 0 aSize
    productHi :: MutableSliced SmallArray s Word64
    productHi = Contiguous.sliceMut product aSize (productSize - aSize)
  -- Multiply each of the digits in a by b, adding them into the 'product' value.
  -- We don't zero out product, because we this will be called multiple times, so it probably contains a previous iteration's partial product, and we're adding + carrying on top of it
  for_ [0..aSize-1] $ \i -> do
    p <- Contiguous.read productLo i
    let aDigit = Contiguous.index a i
    modifySTRef carry (+ Word128 0 p)
    modifySTRef carry (+ (Word128 0 aDigit * Word128 0 b))
    Contiguous.write product i . word128Lo64 =<< readSTRef carry
    modifySTRef carry (`shiftR` 64)

  let productHiSize = productSize - aSize
  for_ [0..productHiSize - 1] $ \i -> do
    p <- Contiguous.read productHi i
    modifySTRef carry (+ Word128 0 p)
    Contiguous.write productHi i . word128Lo64 =<< readSTRef carry
    modifySTRef carry (`shiftR` 64)

  readSTRef carry >>= \case
    0 -> pure ()
    _ -> error "carry overflow during multiplication!"

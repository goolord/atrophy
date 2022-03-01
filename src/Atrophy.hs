{-# LANGUAGE DuplicateRecordFields #-}

module Atrophy where

import Atrophy.LongDivision
import Atrophy.LongMultiplication
import Data.WideWord.Word128
import Data.Word

data StrengthReducedW8 = StrengthReducedW8
  { multiplier :: Word16
  , divisor :: Word8
  }

data StrengthReducedW128 = StrengthReducedW128
  { multiplier_hi :: Word128
  , multiplier_lo :: Word128
  , divisor :: Word128
  }

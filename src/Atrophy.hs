{-# LANGUAGE DuplicateRecordFields #-}

module Atrophy
  ( module REXPORT
  , StrengthReducedW128 (..)
  , StrengthReducedW8 (..)
  , StrengthReducedW64 (..)
  , StrengthReducedW32 (..)
  , StrengthReducedW16 (..)
  ) where

import Atrophy.LongDivision as REXPORT
import Atrophy.LongMultiplication as REXPORT
import Atrophy.Internal as REXPORT
import Data.WideWord.Word128
import Data.Word

data StrengthReducedW8 = StrengthReducedW8
  { multiplier :: !Word16
  , divisor    :: !Word8
  }

data StrengthReducedW128 = StrengthReducedW128
  { multiplierHi :: !Word128
  , multiplierLo :: !Word128
  , divisor      :: !Word128
  }

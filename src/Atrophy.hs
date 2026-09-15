-- | Fast division by divisors that are only known at runtime, via arithmetic
-- strength reduction.
--
-- > let d = new (NonZero 7) :: StrengthReduced Word64
-- > div' 100 d == 14
--
-- Precompute a divisor once with 'new', then each 'divRem' is a multiplication
-- and a few shifts instead of a hardware division.
--
-- For constants, see "Atrophy.Known".
module Atrophy
  ( -- * Nonzero values
    NonZero (..)
  , nonZero
  , getNonZero
    -- * Strength reduction
  , StrengthReduce (..)
  , div'
  , rem'
    -- ** Compile-time numerators
  , divConst
  , remConst
    -- ** Unchecked hardware division
  , divNonZero
  , remNonZero
  , divNonZeroConst
  , remNonZeroConst
    -- ** Precomputed divisors
  , StrengthReducedW128
  , StrengthReducedW64
  , StrengthReducedW32
  , StrengthReducedW16
  , StrengthReducedW8
    -- * Re-exports
  , module Atrophy.Known
  , module Atrophy.LongDivision
  , module Atrophy.LongMultiplication
  ) where

import Atrophy.Internal
import Atrophy.Known
import Atrophy.LongDivision
import Atrophy.LongMultiplication

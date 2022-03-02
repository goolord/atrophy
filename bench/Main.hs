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
#-}

{-# OPTIONS_GHC
    -fno-warn-orphans
#-}

module Main where

import Data.Functor.Identity (Identity(Identity))
import Test.Tasty.Bench (bench, bgroup, defaultMain, env, nf, nfIO)
import Control.DeepSeq (deepseq, NFData (rnf))

main :: IO ()
main = do
  defaultMain
    [ bgroup "atrophy"
        [ bench "div" $ nf (const ()) ()
        ]
    ]

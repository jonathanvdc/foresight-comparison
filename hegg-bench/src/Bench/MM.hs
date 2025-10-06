{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench.MM (benchGroup) where

import Criterion.Main
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler

-- Matrix-multiplication language

data Linalg a
  = Mat Int Int
  | MatMul a a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

data DimAndCost = DimAndCost
  { rows :: Int
  , cols :: Int
  , costVal :: Int
  } deriving (Show, Eq, Ord)

costMM :: CostFunction Linalg DimAndCost
costMM (Mat r c) = DimAndCost r c 0
costMM (MatMul x y) =
  let DimAndCost r1 c1 cost1 = x
      DimAndCost r2 c2 cost2 = y
  in if c1 /= r2
        then error "matrix dimensions do not match"
        else DimAndCost r1 c2 (cost1 + cost2 + r1 * c1 * c2)

rewritesMM :: [Rewrite () Linalg]
rewritesMM =
  [ pat (MatMul (pat (MatMul "x" "y")) "z") := pat (MatMul "x" (pat (MatMul "y" "z"))),
    pat (MatMul "x" (pat (MatMul "y" "z"))) := pat (MatMul (pat (MatMul "x" "y")) "z")
  ]

rewriteMM :: Fix Linalg -> Fix Linalg
rewriteMM expr = fst (equalitySaturation' (BackoffScheduler 0 0) expr rewritesMM costMM)

mm :: Int -> Fix Linalg
mm 0 = Fix (Mat 10 10)
mm n = Fix (MatMul (mm (n - 1)) (Fix (Mat 10 10)))

benchGroup :: Benchmark
benchGroup =
  bgroup "mm"
    [ bench "rewrite mm40" $ whnf rewriteMM (mm 40)
    , bench "rewrite mm80" $ whnf rewriteMM (mm 80)
    ]
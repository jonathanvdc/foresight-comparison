{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench.Poly (benchGroup) where

import Criterion.Main
import Data.Equality.Matching
import Data.Equality.Saturation
import Data.Equality.Saturation.Scheduler
import Data.Equality.Utils

-- Expression language

data Expr a
  = Zero
  | Succ a
  | Var String
  | Add a a
  | Mul a a
  | Pow a a
  deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

succToNum :: Fix Expr -> Int
succToNum (Fix Zero) = 0
succToNum (Fix (Succ x)) = 1 + succToNum x
succToNum _ = error "cannot convert variable to number"

prettyPrint :: Fix Expr -> String
prettyPrint (Fix Zero) = "0"
prettyPrint (Fix (Succ x)) = show (1 + succToNum x)
prettyPrint (Fix (Var v)) = v
prettyPrint (Fix (Add x y)) = "(" ++ prettyPrint x ++ "+" ++ prettyPrint y ++ ")"
prettyPrint (Fix (Mul x y)) = "(" ++ prettyPrint x ++ "*" ++ prettyPrint y ++ ")"
prettyPrint (Fix (Pow x y)) = "(" ++ prettyPrint x ++ "^" ++ prettyPrint y ++ ")"

costPoly :: CostFunction Expr Int
costPoly Zero = 1
costPoly (Succ x) = 1 + x
costPoly (Var _) = 1
costPoly (Add x y) = 10 + x + y
costPoly (Mul x y) = 100 + x + y
costPoly (Pow x y) = 1000 + x + y

rewritesPoly :: [Rewrite () Expr]
rewritesPoly =
  [ pat (Add "x" "y") := pat (Add "y" "x"), -- add-commutative
    pat (Mul "x" "y") := pat (Mul "y" "x"), -- mul-commutative
    pat (Add (pat (Add "x" "y")) "z") := pat (Add "x" (pat (Add "y" "z"))), -- add-associative
    pat (Add "x" (pat (Add "y" "z"))) := pat (Add (pat (Add "x" "y")) "z"), -- add-associative2
    pat (Mul (pat (Mul "x" "y")) "z") := pat (Mul "x" (pat (Mul "y" "z"))), -- mul-associative
    pat (Mul "x" (pat (Mul "y" "z"))) := pat (Mul (pat (Mul "x" "y")) "z"), -- mul-associative2
    pat (Mul "x" (pat (Add "a" "b"))) := pat (Add (pat (Mul "x" "a")) (pat (Mul "x" "b"))), -- distributive
    pat (Add (pat (Mul "x" "a")) (pat (Mul "x" "b"))) := pat (Mul "x" (pat (Add "a" "b"))), -- distributive2
    pat (Mul "x" (pat (Succ (pat Zero)))) := "x", -- x*1 = x
    pat (Pow "x" (pat Zero)) := pat (Succ (pat Zero)), -- x^0 = 1
    pat (Pow "x" (pat (Succ "n"))) := pat (Mul "x" (pat (Pow "x" "n"))) -- x^(n+1) = x * x^n
  ]

rewritePoly :: Fix Expr -> Fix Expr
rewritePoly expr = fst (equalitySaturation' (BackoffScheduler 0 0) expr rewritesPoly costPoly)

-- Helpers to build numbers and variables ---------------------------------------------------

nat :: Int -> Fix Expr
nat n
  | n < 0 = error "nat: negative"
  | n == 0 = Fix Zero
  | otherwise = Fix (Succ (nat (n - 1)))

var :: String -> Fix Expr
var s = Fix (Var s)

x :: Fix Expr
x = var "x"

coeff :: Int -> Fix Expr
coeff k = var ("c" ++ show k)

powN :: Fix Expr -> Int -> Fix Expr
powN base k
  | k < 0 = error "powN: negative exponent"
  | otherwise = Fix (Pow base (nat k))

add :: Fix Expr -> Fix Expr -> Fix Expr
add a b = Fix (Add a b)

mul :: Fix Expr -> Fix Expr -> Fix Expr
mul a b = Fix (Mul a b)

-- Build a polynomial of degree n: c_n x^n + ... + c_1 x + c_0
polyOf :: Int -> Fix Expr
polyOf n
  | n < 0 = error "polyOf: negative degree"
  | n == 0 = coeff 0
  | otherwise = foldr1 add (map term [n, n-1 .. 0])
  where
    term 0 = coeff 0
    term k = mul (coeff k) (powN x k)

benchGroup :: Benchmark
benchGroup =
  bgroup "poly"
    [ bench "rewrite poly5" $ whnf rewritePoly (polyOf 5)
    , bench "rewrite poly6" $ whnf rewritePoly (polyOf 6)
    ]
module Main (main) where

import Criterion.Main
import qualified Bench.Poly as Poly
import qualified Bench.MM   as MM

main :: IO ()
main = defaultMain [ Poly.benchGroup, MM.benchGroup ]
module Shuffle where

import System.Random
import Data.Map

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m,gen) (i,x) = (insert j x . insert i (m ! j) $ m, gen')
  where
    (j,gen') = randomR (0,i) gen

fisherYates :: RandomGen g => g -> [a] -> ([a],g)
fisherYates gen [] = ([], gen)
fisherYates gen (x:xs) 
  = toElems $ Prelude.foldl fisherYatesStep (initial x gen) (numerate xs)
    where
      toElems (p,q) = (elems p, q)
      numerate = zip [1..]
      initial p g = (singleton 0 p, g)

shuffle :: RandomGen g => g -> [a] -> [a]
shuffle g = fst . fisherYates g

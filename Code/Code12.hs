module Code12 where

import Data.Array

-- Specification

tails     ::  [a] -> [[a]]
tails []  =   []
tails xs  =   xs : tails (tail xs)

rank0     ::  Ord a => [a] -> [Int]
rank0 xs  =   map (\ x -> length (filter (< x) xs)) xs

ranktails0  ::  Ord a => [a] -> [Int]
ranktails0  =   rank0 . tails

-- A better algorithm

ranktails0' :: Ord a => [a] -> [Int]
ranktails0' = applyUntil isperm rerankings . rank0

-- rank by psort

rank1  ::  Ord a => [a] -> [Int]
rank1  =   resort . concat . label . psort . zip [0..]

psort      ::  Ord b => [(a,b)] -> [[a]]
psort xys  =   pass xys []

pass :: Ord b => [(a,b)] -> [[a]] -> [[a]]
pass [] xss             = xss
pass ((x,y):xys) xss  = step xys [] [x] [] xss
  where
    step [] as bs cs xss  =  pass as (bs : pass cs xss)
    step (e@(x,y'):xys) as bs cs xss  
      | y' <   y  = step xys (e:as) bs cs xss
      | y' ==  y  = step xys as (x:bs) cs xss
      | y' >   y  = step xys as bs (e:cs) xss

label      ::  [[a]] -> [[(a,Int)]]
label xss  =   zipWith tag xss (scanl (+) 0 (map length xss))

tag xs k   =   [(x,k) | x <- xs]

resort      ::  [(Int,Int)] -> [Int]
resort ijs  =   elems (array (0,length ijs - 1) ijs)

ranktails1  ::  Ord a => [a] -> [Int]
ranktails1  =   rank1 . tails

-- 2nd Algorithm

ranktails2  ::  Ord a => [a] -> [Int]
ranktails2 = applyUntil isperm rerankings . rank1

rerankings :: [[Int] -> [Int]]
rerankings = map rerank (iterate (*2) 1)

rerank :: Int -> [Int] -> [Int]
rerank k rs = rs <<= shiftBy k rs

shiftBy :: Int -> [Int] -> [Int]
shiftBy k rs = map (+k) (drop k rs) ++ [k-1,k-2 .. 0]

(<<=) :: (Ord b, Ord a) => [a] -> [b] -> [Int]
xs <<= ys = rank1 (zip xs ys)

applyUntil :: (a -> Bool) -> [a -> a] -> a -> a
applyUntil p (f:fs) x = if p x then x else applyUntil p fs (f x)

isperm  ::  [Int] -> Bool
isperm is  
  = and (elems (accumArray (||) False (0,n-1) (zip is (repeat True))))
    where  n = length is

-- final algorithm

ranktails3     ::  Ord a => [a] -> [Int]
ranktails3 xs  =   ( resort3 n . concat . label3
                   . applyUntil (all single) (repartitions3 n)
                   . psort . zip [0..]) xs
                   where  n = length xs

single      ::  [a] -> Bool
single [_]  =   True
single _    =   False

resort3 n  =  elems . array (0,n-1)
label3 iss = zipWith tag3 iss (scanl (+) 0 (map length iss))
tag3 is j = [(i,j)| i <- is]

repartitions3 n = map (repartition3 n) (iterate (*2) 1)
repartition3 n k iss = concatMap (psort . map install) iss
  where
    install i = (i,if j < n then k + a!j else n - i - 1)
      where j = i+k
    a         = array (0,n-1) (concat (label iss))

--
sample  ::  [Int]
sample  =   [51,38,29,51,63,38]

s1,s2  :: [Int]
s1  =  [3,1,3,0,1]
s2  =  [2,0,3,4,0]

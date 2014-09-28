module Code13 where

import Data.Array
import Data.Char
import Data.List
import Data.Ord

transform0     ::  Ord a => [a] -> ([a],Int)
transform0 xs  =   (map last xss, position xs xss)
                  where  xss = sort (rots xs)

position :: Eq a => a -> [a] -> Int
position xs xss = length (takeWhile (/= xs) xss)

rots     ::  [a] -> [[a]]
rots xs  =   take (length xs) (iterate lrot xs)
             where  lrot (x:xs) = xs ++ [x]
                    lrot _      = error "rots is not defined on empty list"

lrot :: [a] -> [a]
lrot (x:xs) = xs ++ [x]
lrot _      = error "lrot is not defined on empty list"

--

recreate :: Int -> [[a]] -> [[a]]
recreate 0 = map (const [])

takeCols    ::  Int -> [[a]] -> [[a]]
takeCols j  =   map (take j)

rrot     ::  [a] -> [a]
rrot xs  =   [last xs] ++ init xs

hdsort  ::  Ord a => [[a]] -> [[a]]
{-
hdsort  =   sortBy cmp where cmp (x:xs) (y:ys) = compare x y
-}
hdsort  =   sortBy (comparing head)

consCol           ::  Ord a => ([a],[[a]]) -> [[a]]
consCol (xs,xss)  =   zipWith (:) xs xss

--

untransform         ::  (Ord a, Num a) => ([a],Int) -> [a]
untransform (ys,k)  =   take n (tail (map (ya!) (iterate (pa!) k)))
  where  n   = length ys
         ya  = listArray (0,n-1) ys
         pa  = listArray (0,n-1) (map snd (sort (zip ys [0..])))

transform     ::  (Ord a, Num a) => [a] -> ([a],Int)
{-
transform xs  =   ([xa ! i | i <- ps], k)
  where  n   =  length xs
         k   =  length (takeWhile (/= 0) ps)
         xa  =  listArray (0,n-1) (rrot xs)
         ps  =  map snd (sort (zip (tails (tag xs)) [0 .. n-1]))
-}

transform xs = ([ x | i <- ps, let x = xa ! i, x /= eof ],k-1)
  where  n   = length xs
         k   = length (takeWhile (/= 0) ps)
         xa  = listArray (0,n) (rrot xs')
         xs' = tag xs
         ps  = map snd (sort (zip (tails xs') [0..n]))

tag     ::  Num a => [a] -> [a]
tag xs  =   xs ++ [eof]


eof :: Num a => a
eof = -1

--

sample0 = map ord "yokohama"
sample1 = [2,1,1,1,1]
sample2 = [3,2,1,2]
sample3 = [1..5]
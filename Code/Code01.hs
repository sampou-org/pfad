module Code01 where

import Data.Array
import Data.Array.ST

import Data.List (partition)

-- Sample data

sampleX  ::  [Int]
sampleX  = [08,23,09,00,12,11,01,10,13,07,41,04,14,21,05,17,03,19,02,06]

-- Specification

minfree0     ::  [Int] -> Int
minfree0 xs  =   head ([0..] \\ xs)

(\\)      ::  Eq a => [a] -> [a] -> [a]
us \\ vs  =   filter (`notElem` vs) us

-- Implementation using Array (ver. 1)

minfreeA1  ::  [Int] -> Int
minfreeA1  =   search1 . checklist

search1  ::  Array Int Bool -> Int
search1  =   length . takeWhile id . elems

checklist     ::  [Int] -> Array Int Bool
checklist xs  =   accumArray (||) False (0,n) (zip (filter (<= n) xs) (repeat True))
                  where n = length xs

-- Implementation using Array (ver. 2)

minfreeA2  ::  [Int] -> Int
minfreeA2  =   search2 . countlist

search2  ::  Array Int Int -> Int
search2  =   fst . head . dropWhile ((0/=) . snd) . assocs 

countlist     ::  [Int] -> Array Int Int
countlist xs  =   accumArray (+) 0 (0,n) (zip xs (repeat 1))
                  where  n = maximum xs

-- Implementation using STArray (ver. 3) (procedural way!)

minfreeA3  ::  [Int] -> Int
minfreeA3  =   search1 . checklist'

checklist'     ::  [Int] -> Array Int Bool
checklist' xs  =   runSTArray (do 
                     { a <- newArray (0,n) False;
                       sequence [writeArray a x True | x <- xs, x <= n];
                       return a })
                   where n = length xs

--

minfree     ::  [Int] -> Int
minfree xs  =   minform 0 (length xs, xs)

minform         ::  Int -> (Int, [Int]) -> Int
minform a (n,xs)
  | n == 0      =   a
  | m == b - a  =   minform b (n-m, vs)
  | otherwise   =   minform a (m, us)
    where  (us,vs)  =  partition (< b) xs
           b        =  a + 1 + n `div` 2
           m        = length us


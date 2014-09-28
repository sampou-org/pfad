module Code02 where

import Data.List (sort)

-- Sample Data

sample  ::  String
sample  =   "GENERATING"

-- Specification 

msc0     ::  Ord a => [a] -> Int
msc0 xs  =   maximum [scount z zs | z:zs <- tails xs]

scount       ::  Ord a => a -> [a] -> Int
scount x xs  =   length (filter (x <) xs)

tails             ::  [a] -> [[a]]
tails []          =   []
tails xxs@(_:xs)  =   xxs : tails xs

-- table

msc1  ::  Ord a => [a] -> Int
msc1  =   maximum . map snd . table1

table1  ::  Ord a => [a] -> [(a, Int)]
table1  =   tableSpec1

tableSpec1     ::  Ord a => [a] -> [(a, Int)]
tableSpec1 xs  =   [(z,scount z zs) | z:zs <- tails xs]

-- join

join2          ::  Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join2 txs tys  =   [(z,c+tcount2 z tys)|(z,c) <- txs] ++ tys

tcount2        ::  Ord a => a -> [(a, b)] -> Int
tcount2 z tys  =   scount z (map fst tys)

tableSpec2  ::  Ord a => [a] -> [(a, Int)]
tableSpec2  =   tableSpec1

-- sorted table

tableSpec3     ::  Ord a => [a] -> [(a,Int)]
tableSpec3 xs  =   sort [(z,scount z zs) | z:zs <- tails xs]

table3  ::  Ord a => [a] -> [(a,Int)]
table3 [x] = [(x,0)]
table3 xs  = join3 (table3 ys) (table3 zs)
  where  m        = length xs `div` 2
         (ys,zs)  = splitAt m xs

join3          ::  Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join3 txs tys  =   [(x,c + tcount3 x tys) | (x,c) <- txs] /\/\ tys
  where  []   /\/\ tbs  = tbs
         tas  /\/\ []   = tas
         tas@(a:as)  /\/\ tbs@(b:bs)
             | fst a < fst b  = a : (as /\/\ tbs)
             | otherwise      = b : (tas /\/\ bs)

tcount3        ::  Ord a => a -> [(a,Int)] -> Int
tcount3 z tys  =   length (dropWhile ((z >=) . fst) tys)

-- merge and count

tableSpec4  ::  Ord a => [a] -> [(a,Int)]
tableSpec4  =   tableSpec3

table4  ::  Ord a => [a] -> [(a,Int)]
table4 [x] = [(x,0)]
table4 xs  = join4 (table4 ys) (table4 zs)
  where  m        = length xs `div` 2
         (ys,zs)  = splitAt m xs

join4          ::  Ord a => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join4 []  tys  =   tys
join4 txs []   =   txs
join4 txs@((x,c):txs') tys@((y,d):tys')
  | x < y      = (x,c + length tys)  : join4 txs'  tys
  | otherwise  = (y,d)               : join4 txs   tys'

-- Final implementation

msc  ::  Ord a => [a] -> Int
msc  =   maximum . map snd . table

table :: Ord a => [a] -> [(a, Int)]
table [x]  =   [(x,0)]
table xs   =   join (m-n) (table ys) (table zs)
               where  m        = length xs
                      n        = m `div` 2
                      (ys,zs)  = splitAt n xs

join           ::  Ord a => Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join 0 txs []  =   txs
join _ [] tys  =   tys
join n txs@((x,c):txs') tys@((y,d):tys')
  | x <   y  =  (x,c+n)  : join n      txs'  tys
  | x >=  y  =  (y,d)    : join (n-1)  txs   tys'

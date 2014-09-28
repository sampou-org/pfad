module Code05 where

import Data.Array
import Data.List (sort,sortBy)

-- Specification

type A = Int

(⊕)    ::  A -> A -> A
x ⊕ y  =   x + y

(⊖)    ::  A -> A -> A
x ⊖ y  =   x ⊕ (negate y)

sortsums0 :: [A] -> [A] -> [A]
sortsums0 xs ys = sort [x ⊕ y | x <- xs, y <- ys ]

--

type Label a = (a, (Int,Int))

subs1  ::  [A] -> [A] -> [Label A]
subs1 xs ys  =   [(x ⊖ y, (i,j)) | (x,i) <- zip xs [1..], (y,j) <- zip ys [1..]]

sortsums1        ::  [A] -> [A] -> [A]
sortsums1 xs ys  =   map fst (sortsubs1 xs (map negate ys))

sortsubs1        ::  [A] -> [A] -> [(A,(Int,Int))]
sortsubs1 xs ys  =   sort (subs1 xs ys)


table1             ::  [A] -> [A] -> [(Int,Int,Int)]
table1 xs ys       =   map snd (map (tag1 1) xxs /\/\ map (tag1 2) yys)
                      where  xxs  = sortsubs1 xs xs
                             yys  = sortsubs1 ys ys

tag1              ::  Int -> (A,(Int,Int)) -> (A,(Int,Int,Int))
tag1 i (x,(j,k))  =   (x,(i,j,k))

(/\/\) :: Ord a => [a] -> [a] -> [a]
[] /\/\ ys = ys
xs /\/\ [] = xs
xxs@(x:xs) /\/\ yys@(y:ys)
  | x <= y    = x : (xs /\/\ yys)
  | otherwise = y : (xxs /\/\ ys)

-- Final implementation 

sortsums        ::  [A] -> [A] -> [A]
sortsums xs ys  =   map fst (sortsubs xs (map negate ys))

sortsubs        ::  [A] -> [A] -> [(A, (Int, Int))]
sortsubs xs ys  =   sortBy (cmp (mkArray xs ys)) (subs xs ys)

subs        ::  [A] -> [A] -> [(A, (Int, Int))]
subs xs ys  =   [(x ⊖ y,(i,j)) | (x,i) <- zip xs [1..], (y,j) <- zip ys [1..]]

cmp :: Array (Int,Int,Int) Int -> (A, (Int,Int)) -> (A, (Int, Int)) -> Ordering
cmp a (_,(i,j)) (_,(k,l))  =  compare (a!(1,i,k)) (a!(2,j,l))

mkArray          :: [A] -> [A] -> Array (Int,Int,Int) Int
mkArray xs ys    =  array b (zip (table xs ys) [1..])
                    where  b  =  ((1,1,1),(2,p,p))
                           p  =  max (length xs) (length ys)

table            ::  [A] -> [A] -> [(Int,Int,Int)]
table xs ys      =   map snd (map (tag 1) xxs /\/\ map (tag 2) yys)
                     where  xxs  =  sortsubs' xs
                            yys  =  sortsubs' ys

tag              ::  Int -> (A,(Int,Int)) -> (A,(Int,Int,Int))
tag i (x,(j,k))  =   (x,(i,j,k))

sortsubs'      ::  [A] -> [(A, (Int, Int))]
sortsubs' []   =   []
sortsubs' [w]  =   [(w ⊖ w,(1,1))]
sortsubs' ws   =   foldr1 (/\/\) [  xxs,map (incr m) xys,
                                   map (incl m) yxs, map (incb m) yys ]
  where  xxs      =  sortsubs' xs
         xys      =  sortBy (cmp (mkArray xs ys)) (subs xs ys)
         yxs      =  map switch (reverse xys)
         yys      =  sortsubs' ys
         (xs,ys)  =  splitAt m ws
         m        =  length ws `div` 2

incl,incr,incb     ::  Int -> (A,(Int,Int)) -> (A,(Int,Int))
incl  m (x,(i,j))  =   (x,(m+i,j))
incr  m (x,(i,j))  =   (x,(i,m+j))
incb  m (x,(i,j))  =   (x,(m+i,m+j))

switch            ::  (A,(Int,Int)) -> (A,(Int,Int))
switch (x,(i,j))  =   (negate x,(j,i))

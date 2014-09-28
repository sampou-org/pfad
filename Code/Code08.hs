module Code08 where

import Data.Function (on)
import Data.List (groupBy, minimumBy,sort,sortBy)
import Data.Ord (comparing)

minBy        ::  Ord b => (a -> b) -> [a] -> a
minBy f      =   foldl1 (cmp f)
cmp :: Ord b => (a -> b) -> a -> a -> a
cmp f u v    =   if f u <= f v then u else v

up             ::  Ord a => [a] -> Bool
up []          =   True
up xs@(_:xs')  =   and (zipWith (<) xs xs')

-- specification

supravel0  ::  Ord a => [a] -> [[a]]
supravel0  =   minBy length . filter (all up) . unravels0

unravels0             ::  [a] -> [[[a]]]
unravels0             =   foldr (concatMap . prefixes0) [[]]

prefixes0             ::  a -> [[a]] -> [[[a]]]
prefixes0 x []        =   [[[x]]]
prefixes0 x (xs:xss)  =   [(x:xs):xss] ++ map (xs:) (prefixes0 x xss)

-- Deriving 1

supravel1  ::  Ord a => [a] -> [[a]]
supravel1  =   minBy length . upravels1

{-
upravels    =   filter (all up) . unravels
-}

upravels1  ::  Ord a => [a] ->  [[[a]]]
upravels1  =   foldr (concatMap . uprefixes1) [[]]

uprefixes1             ::  Ord a => a -> [[a]] -> [[[a]]]
uprefixes1 x []        =   [[[x]]]
uprefixes1 x (xs:xss)  =   if x <= head xs
                             then [(x:xs):xss] ++ map (xs:) (uprefixes1 x xss)
                             else map (xs:) (uprefixes1 x xss)

-- Deriving 2

minWith  :: (a -> a -> Bool) -> [a] -> a
minWith c [x] = x
minWith c (x:y:ys) | c x y     = minWith c (x:ys)
                   | otherwise = minWith c (y:ys)

ur <|~ vr = heads ur <|= heads vr

(<|=)      ::  Ord a => [a] -> [a] -> Bool
xs <|= ys  | m <= n && and (zipWith (>=) xs ys) = True
           | otherwise                          = False
  where
    m  = length xs
    n  = length ys

heads  ::  Ord a => [[a]] -> [a]
heads  =   sort . map head

insert             ::  Ord a => a -> [[a]] -> [[a]]
insert x []        =  [[x]]
insert x (xs:xss)  =  if x <= head xs then (x:xs):xss
                      else xs : insert x xss

supravel  ::  Ord a => [a] -> [[a]]
supravel  =   foldr insert []

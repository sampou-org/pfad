module Code10 where

import Data.List hiding (nub,insert)
import qualified Data.List as L (nub)

import Data.Set (Set, empty,member,split,elems,insert)

{-
nub  ::  Ord a => [a] -> [a]
nub  =   minimum . longest . filter nodups . subseqs
-}

nub0         ::  Ord a => [a] -> [a]
nub0 []      =   []
nub0 (x:xs)  =   if x `notElem` xs then x : nub0 xs else
                 (x : nub0 (xs \\ [x])) `min` (nub0 xs)

nub1 :: Ord a => [a] -> [a]
nub1 = hub1 []

hub1 ws []      = []
hub1 ws (x:xs)  =  case (x `elem` xs, x `elem` ws) of
                     (False,False)  -> us++[x]++hub1 [] (xs\\us)
                     (False,True)   -> us++[x]++hub1 (tail vs) (xs\\us)
                     (True,False)   -> hub1 (us++[x]) xs
                     (True,True)    -> hub1 ws xs
                   where
                     (us,vs) = span (<x) ws

-- Introducing Sets

nub2 :: Ord a => [a] -> [a]
nub2 = hub2 empty . preprocess2

preprocess2 xs = zip xs (tail (scanr insert empty xs))
hub2 ws [] = []
hub2 ws ((x,xs):xss) =
  case (member x xs, member x ws) of
    (False,False) -> eus++[x]++hub2 empty yss
  where
    (us,vs) = split x ws
    eus     = elems us
    yss     = [(x,xs)| (x,xs) <- xss, not (member x us)]


-- Final version

nub            ::  Ord a => [a] -> [a]
nub            =   hub' empty empty . preprocess

preprocess     ::  Ord a => [a] -> [(a, Set a)]
preprocess xs  =   zip xs (tail (scanr insert empty xs))

hub' :: Ord a => Set a -> Set a -> [(a, Set a)] -> [a]
hub' _  _  []  = []
hub' ps ws ((x,xs):xss)
  | member x ps  =  hub' ps ws xss
  | otherwise    =  case (member x xs, member x ws) of
      (False,False)  -> eus ++ [x] ++ hub' qs empty xss
      (False,True)   -> eus ++ [x] ++ hub' qs vs xss
      (True,False)   -> hub' ps (insert x us) xss
      (True,True)    -> hub' ps ws xss
    where  (us,vs)  = split x ws
           eus      = elems us
           qs       = foldr insert ps eus

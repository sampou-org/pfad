module Code29_Loopless where

import Data.List hiding (insert)
import Code28

{-
bumpDn            =   unfoldr stepDn . prologDn

prologDn (k,n)    =   (k,k,n-1,1)
stepDn (j,k,m,n)  =   if m < n then Nothing
                      else Just (m+j,(k-j,k,m-1,n))

prologUp (k,n)    =   (if even n then k else 0,k,1,n-1)
stepUp (j,k,m,n)  =   if m > n then Nothing
                      else Just (m+j,(k-j,k,m+1,n))
-}

jcode  ::  Int -> [Int]
jcode  =   unfoldr step . wrapQueue . fst . foldr op' (empty,empty) . pairs
           where  pairs n = addpair (0,n) []

addpair           ::  (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
addpair (_,1) ps  =   ps
addpair (k,n) ps  =   addpair (k',n-1) ((k,n):ps)
                      where  k' = if odd n then k+1 else 1

mix                 ::  [a] -> (Queue (Rose a), Queue (Rose a)) -> Queue (Rose a)
mix []     (ys,_)   =   ys
mix (x:xs) (ys,sy)  =   insert ys (Node x (mix xs (sy,ys)))

step           ::  [Forest a] -> Maybe (a, [Forest a])
step []        =   Nothing
step (zs:zss)  =   Just (x,consQueue xs (consQueue ys zss))
                   where  (Node x xs, ys) = remove zs

consQueue         ::  Queue a -> [Queue a] -> [Queue a]
consQueue xs xss  = if isempty xs then xss else xs:xss

wrapQueue         ::  Queue a -> [Queue a]
wrapQueue xs      =   consQueue xs []

bump :: (Int,Int,Int,Int,Int) -> Maybe (Int,(Int,Int,Int,Int,Int))
bump (i,j,k,m,n)  =   if i*(n-m) < 0 then Nothing
                      else Just (m+j,(i,k-j,k,m+i,n))

prologDn        ::  (Int,Int) -> (Int,Int,Int,Int,Int)
prologDn (k,n)  =   (-1,k,k,n-1,1)
prologUp        ::  (Int,Int) -> (Int,Int,Int,Int,Int)
prologUp (k,n)  =   (1,if even n then k else 0,k,1,n-1)

op' :: (Int, Int) -> (Forest Int,Forest Int) -> (Forest Int,Forest Int)
op' (k,n) (ys,sy)
  =  if odd n
     then  (  mix (unfoldr bump (-1,k,k,n-1,1)) (ys,sy),
              mix (unfoldr bump (1,0,k,1,n-1)) (sy,ys))
     else  (  mix (unfoldr bump (-1,k,k,n-1,1)) (ys,sy),
              mix (unfoldr bump (1,k,k,1,n-1)) (ys,sy))

module Code04 where

import Data.Array

-- Sample data

sX,sY :: Int -> [Int]
sX n = take n [ 2*x | x <- [0..], x `mod` 3 /= 0 ]
sY n = take n [ 3*x | x <- [0..]]
aX,aY :: Int -> Array Int Int
aX n = listArray (0,n) (sX n)
aY n = listArray (0,n) (sY n)


sX10 = sX 10
sX100 = sX 100
sX1000 = sX 1000
sX10000 = sX 10000
sX100000 = sX 100000
sX1000000 = sX 1000000
sX10000000 = sX 10000000

sY10 = sY 10
sY100 = sY 100
sY1000 = sY 1000
sY10000 = sY 10000
sY100000 = sY 100000
sY1000000 = sY 1000000
sY10000000 = sY 10000000

aX10 = aX 10
aX100 = aX 100
aX1000 = aX 1000
aX10000 = aX 10000
aX100000 = aX 100000
aX1000000 = aX 1000000
aX10000000 = aX 10000000

aY10 = aY 10
aY100 = aY 100
aY1000 = aY 1000
aY10000 = aY 10000
aY100000 = aY 100000
aY1000000 = aY 1000000
aY10000000 = aY 10000000


-- Specification

smallest0            ::  Ord a => Int -> ([a],[a]) -> a
smallest0 k (xs,ys)  =   union (xs,ys) !! k

union          ::  Ord a => ([a],[a]) -> [a]
union (xs,[])  =   xs
union ([],ys)  =   ys
union (x:xs,y:ys) | x < y  =  x : union (xs,y:ys)
                  | x > y  =  y : union (x:xs,ys)

-- Implementation

smallest ::  Ord a => Int -> ([a],[a]) -> a
smallest k ([],ws) = ws !! k
smallest k (zs,[]) = zs !! k
smallest k (zs,ws)
  = case (a < b, k <= p + q) of
      (True,True)    ->  smallest k (zs,us)
      (True,False)   ->  smallest (k-p-1) (ys,ws)
      (False,True)   ->  smallest k (xs,ws)
      (False,False)  ->  smallest (k-q-1) (zs,vs)
    where
      p  = length zs `div` 2
      q  = length ws `div` 2
      (xs,a:ys)  =  splitAt p zs
      (us,b:vs)  =  splitAt q ws

-- Implementation using Array

smallestA            ::  Ord a => Int -> (Array Int a,Array Int a) -> a
smallestA k (xa,ya)  =   search k (0,m+1) (0,n+1)
  where
    (0,m)  = bounds xa
    (0,n)  = bounds ya 
    search k (lx,rx) (ly,ry)
      | lx == rx   =  ya ! (k+ly)
      | ly == ry   =  xa ! (k+lx)
      | otherwise  =  case (xa ! mx < ya ! my, k <= mx-lx + my-ly) of
                        (True,True)    -> search k (lx,rx) (ly,my)
                        (True,False)   -> search (k-(mx-lx)-1) (mx+1,rx) (ly,ry)
                        (False,True)   -> search k (lx,mx) (ly,ry)
                        (False,False)  -> search (k-(my-ly)-1) (lx,rx) (my+1,ry)
                      where
                         mx  = (lx+rx) `div` 2
                         my  = (ly+ry) `div` 2

module Code15 where

import Queue 
import Data.Array hiding (elems)
import Data.List hiding (tails,insert)
import qualified Data.List  as L (tails)

sample :: String
sample = "abacabacab"

tails :: [a] -> [[a]]
tails = init . L.tails

allcp0 :: Eq a => [a] -> [Int]
allcp0 xs = map (llcp0 xs) (tails xs)

llcp0        ::  Eq a => [a] -> [a] -> Int
llcp0 xs ys  =   length (takeWhile (uncurry (==)) (zip xs ys))

--

allcp1 :: Eq a => [a] -> [Int]
allcp1 xs = fst4 (until (done n) (step1 xs) ([n],0,0,1))
            where  n = length xs

done :: Eq a => Int -> ([a],Int,Int,Int) -> Bool
done n (_,_,_,k)  =  k == n

step1 :: Eq a => [a] -> ([Int],Int,Int,Int)
                     -> ([Int],Int,Int,Int)
step1 xs (as,i,p,k)
   |  k > i + p  =  (snoc as a,k,a,k+1)
   |  q /= r     =  (snoc as (min q r),i,p,k+1)
   |  q == r     =  (snoc as b,k,b,k+1)
      where  q  =  as !! (k-i)
             r  =  p - (k-i)
             a  =  llcp1 xs (drop k xs)
             b  =  q + llcp1 (drop q xs) (drop (q+k) xs)

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snoc :: [a] -> a -> [a]
snoc xs x      = xs ++ [x]

llcp1 :: Eq a => [a] -> [a] -> Int
llcp1 _ []           =  0
llcp1 [] _           =  0
llcp1 (x:xs) (y:ys)  =  if x == y then 1 + llcp1 xs ys else 0

--

allcp xs = extract (until done step (as, empty, 0, 1))
  where
    extract (as,qs,h,k)  =  elems as
    done (as,qs,h,k)     =  (k == n)
    n                    =  length xs
    as                   =  insert empty n
    xa                   =  listArray (0, n-1) xs
    step (as,qs,h,k)  | k >=  h = (insert as a,insert as' a,k+a,k+1)
                      | q /=  r = (insert as m,insert qs' m,h,k+1)
                      | q ==  r = (insert as b,insert as' b,k+b,k+1)
                        where as'      = snd (remove as)
			      (q,qs')  = remove qs
                              r        = h - k
			      m        = min q r
			      a        = llcp' 0 k
			      b        = q + llcp' q (q+k)
    llcp' j k  | j == n || k == n  = 0
               | xa ! j == xa ! k  = 1 + llcp' (j+1) (k+1)
               | otherwise         = 0

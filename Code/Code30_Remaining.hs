module Code30_Remaining where

import Data.List (unfoldr)
import Data.Sequence hiding (empty,unfoldr,reverse,length,insert)
import qualified Data.Sequence as S
import Code30_Loopless hiding (bop,wop)
import Code30_Parity hiding (bop,wop)

type Forest a  =  Queue (Rose a)
data Rose a    =  Fork a (Forest a)

ncode  =  unfoldr step . prolog
step   =  undefined

prolog :: [Spider] -> [Forest Int]
prolog =  wrapQueue . fst . foldr bop (empty,empty) . map (Dn' . decorate)

bop,wop :: Leg' -> (Forest Int,Forest Int) -> (Forest Int, Forest Int)
bop  (Up' (Node' (_,b) _ legs)) ps
  =  swap (foldr bop (swapif b ps) legs)
bop  (Dn' (Node' (w,_) a legs)) ps
  =  cat a (foldr wop (swapif (not w) ps) legs) (foldr bop (swapif w ps) legs)
wop  (Up' (Node' (_,b) a legs)) ps
  =  cat a (foldr wop (swapif b ps) legs) (foldr bop (swapif (not b) ps) legs)
wop  (Dn' (Node' (w,_) _ legs)) ps
  =  swap (foldr wop (swapif w ps) legs)

cat :: a -> (Forest a,Forest a) -> (Forest a, Forest a) -> (Forest a, Forest a)
cat a (ws,sw) (bs,sb)  =  (insert ws (Fork a bs), insert sb (Fork a sw))

swap (xs, ys)     =  (ys, xs)
swapif b (xs,ys)  =  if b then (ys,xs) else (xs,ys)


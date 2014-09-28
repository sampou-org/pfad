module Code29 where

import Data.List (unfoldr)
import Data.Sequence hiding (null,empty,length,unfoldr)
import qualified Data.Sequence as S

jcode :: Int -> [Int]
jcode = unfoldr step . prolog

prolog :: Int -> [Forest (Int, State)]
prolog = wrapQueue . fst . foldr op' (empty,empty) . pairs

pairs :: Int -> [(Int, Int)]
pairs n = addpair (0,n) []

addpair           ::  (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
addpair (_,1) ps  =   ps
addpair (k,n) ps  =   addpair (k',n-1) ((k,n):ps)
                      where  k' = if odd n then k+1 else 1

type Queue    = Seq

type Forest a = Queue (Rose a)
data Rose a   = Node a (Forest a, Forest a)

type State    = (Int,Int,Int,Int,Int)
type Pair a   = (a,a)

step  ::  [Forest (Int,State)] -> Maybe (Int,[Forest (Int,State)])
step [] = Nothing
step (zs:zss)
  =  Just (x,consQueue (mix q (sy,ys)) (consQueue zs' zss))
     where (Node (x,q) (ys,sy),zs')  =  remove zs

mix :: State -> Pair (Forest (Int,State)) -> Forest (Int,State)
mix (i,j,k,m,n) (ys,sy)
  =  if i*(n-m) < 0 then ys
     else insert ys (Node (m+j,(i,k-j,k,m+i,n)) (ys,sy))

op'                ::  (Int,Int) -> Pair (Forest (Int,State))
                       -> Pair (Forest (Int,State))
op' (k,n) (ys,sy)  =   if odd n
                       then  (  mix (-1,k,k,n-1,1) (ys,sy),
                                mix (1,0,k,1,n-1) (sy,ys))
                       else  (  mix (-1,k,k,n-1,1) (ys,sy),
                                mix (1,k,k,1,n-1) (ys,sy))

-- Queue

insert   :: Queue a -> a -> Queue a
remove   :: Queue a -> (a, Queue a)
empty    :: Queue a
isempty  :: Queue a -> Bool

insert     = (|>)
remove q   = case viewl q of
  x :< q'  -> (x,q')
  _        -> error "empty queue"
empty      = S.empty
isempty    = S.null

consQueue :: Queue a -> [Queue a] -> [Queue a]
consQueue xs xss  =   if isempty xs then xss else xs : xss

wrapQueue :: Queue a -> [Queue a]
wrapQueue xs = consQueue xs []

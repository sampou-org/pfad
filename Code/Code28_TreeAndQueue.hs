module Code28_TreeAndQueue (Queue,Forest,Rose (..)
                           ,insert,remove,empty,isempty
                           ,boxall1,boxall2) where

import Data.List (unfoldr)
import Data.Sequence hiding (null,empty,unfoldr,length,reverse)
import qualified Data.Sequence as S

consList         :: [a] -> [[a]] -> [[a]]
consList xs xss  =   if null xs then xss else xs:xss

-- Tree

type Forest' a  =  [Rose' a]
data Rose' a    =  Node' a (Forest' a)

preorder  ::  Forest' a -> [a]
preorder  =   unfoldr step . wrapList
  where
    step                        ::  [Forest' a] -> Maybe (a,[Forest' a])
    step []                     =   Nothing
    step ((Node' x xs:ys):zss)  =   Just (x,consList xs (consList ys zss))

wrapList :: [a] -> [[a]]
wrapList xs = consList xs []

op1'                 ::  [a] -> (Forest' a, Forest' a) -> (Forest' a, Forest' a)
op1' [] (ys,sy)      =   (ys, sy)
op1' (x:xs) (ys,sy)  =   (ys ++ [Node' x zs],sz ++ [Node' x sy])
                         where (zs,sz) = op1' xs (sy, ys)

op2'             ::  [a] -> (Forest' a, Forest' a) -> (Forest' a, Forest' a)
op2' xs (ys,sy)  =   if even (length xs)
                     then (mix' xs (ys,sy),mix' (reverse xs) (sy,ys))
                     else (mix' xs (ys,sy),mix' (reverse xs) (ys,sy))

mix' :: [a] -> (Forest' a, Forest' a) -> Forest' a
mix' []     (ys,_)   =  ys
mix' (x:xs) (ys,sy)  =  ys ++ [Node' x (mix' xs (sy,ys))]

-- Queue

type Queue a   = Seq a
type Forest a  = Queue (Rose a)
data Rose a    = Node a (Forest a)

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

boxall1,boxall2 :: [[a]] -> [a]
boxall1 = unfoldr step . wrapQueue . fst . foldr op1 (empty, empty)
boxall2 = unfoldr step . wrapQueue . fst . foldr op2 (empty, empty)

op1,op2 :: [a] -> (Forest a, Forest a) -> (Forest a, Forest a)
op1 []     (ys,sy)  =  (ys,sy)
op1 (x:xs) (ys,sy)  =  (insert ys (Node x zs), insert sz (Node x sy))
                       where  (zs,sz)  =  op1 xs (sy,ys)

op2 xs (ys,sy)  =  if even (length xs) 
                   then (mix xs (ys,sy), mix (reverse xs) (sy,ys))
                   else (mix xs (ys,sy), mix (reverse xs) (ys,sy))

mix :: [a] -> (Forest a, Forest a) -> Forest a
mix []     (ys,_)   =  ys
mix (x:xs) (ys,sy)  =  insert ys (Node x (mix xs (sy,ys)))


step           ::  [Forest a] -> Maybe (a, [Forest a])
step []        =   Nothing
step (zs:zss)  =   Just (x,consQueue xs (consQueue ys zss))
                   where (Node x xs,ys) = remove zs

consQueue :: Queue a -> [Queue a] -> [Queue a]
consQueue xs xss  =   if isempty xs then xss else xs : xss

wrapQueue :: Queue a -> [Queue a]
wrapQueue xs = consQueue xs []

module Code30_General where

import Data.Sequence hiding (empty,unfoldr,reverse)
import qualified Data.Sequence as S

(□)      ::  [a] -> [a] -> [a]
xs □ ys  =   mix xs (ys,reverse ys)

mix                 ::  [a] -> ([a],[a]) -> [a]
mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)

(◇)      ::  [a] -> [a] -> [a]
as ◇ bs  =   reverse (reverse as □ reverse bs)

boxall  ::  [[a]] -> [a]
boxall  =   foldr (□) []

coxall  ::  [[a]] -> [a]
coxall  =   foldr (◇) []

type Nest    =  [Spider]
data Spider  =  Node Int [Leg]
data Leg     =  Dn Spider | Up Spider

ncode  ::  Nest -> [Int]
ncode  =   bcode . map Dn

bcode,wcode  ::  [Leg] -> [Int]
bcode        =   boxall . map bc
wcode        =   coxall . map wc

bc,wc                  ::  Leg -> [Int]
bc (Up (Node _ legs))  =   reverse (bcode legs)
bc (Dn (Node a legs))  =   wcode legs ++ [a] ++ bcode legs
wc (Up (Node a legs))  =   wcode legs ++ [a] ++ bcode legs
wc (Dn (Node _ legs))  =   reverse (wcode legs)

-- Queue

type Queue    = Seq
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

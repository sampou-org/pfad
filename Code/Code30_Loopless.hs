module Code30_Loopless where

import Data.Sequence hiding (empty,unfoldr,reverse,length)
import qualified Data.Sequence as S

type Nest    =  [Spider]
data Spider  =  Node Int [Leg]
data Leg     =  Dn Spider | Up Spider

ncode                      ::  Nest -> [Int]
ncode                      =   foldr bop [] . map Dn

bop,wop                    ::  Leg -> [Int] -> [Int]
bop (Up (Node _ legs)) cs  =   reverse (foldr bop cs' legs)
  where  cs' = if even (length (foldr bop [] legs)) then reverse cs else cs
bop (Dn (Node a legs)) cs  =   foldr wop (reverse cs') legs ++ [a] ++ foldr bop cs' legs
  where  cs' = if even (length (foldr wop [] legs)) then reverse cs else cs
wop (Up (Node a legs)) cs  =   foldr wop cs' legs ++ [a] ++ foldr bop (reverse cs') legs
  where  cs' = if even (length (foldr bop [] legs)) then reverse cs else cs
wop (Dn (Node _ legs)) cs  =   reverse (foldr wop cs' legs)
  where  cs' = if even (length (foldr wop [] legs)) then reverse cs else cs

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

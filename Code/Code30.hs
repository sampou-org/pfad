{-# LANGUAGE EmptyDataDecls #-}
module Code30 where

import Data.List (unfoldr)
{-
type Nest    =  [Spider]
data Spider  =  Node Int Nest
-}
{-
ncode              ::  Nest -> [Int]
ncode              =   boxall . map scode

scode              ::  Spider -> [Int]
scode (Node a xs)  =   a : ncode xs

-- boustrophedon product

preorder                 ::  Nest -> [Int]
preorder []              =   []
preorder (Node n xs:ns)  =   preorder xs ++ n : preorder ns

boxall  ::  [[Int]] -> [Int]
boxall  =   preorder . fst . foldr op ([],[])

op                 ::  [Int] -> (Nest, Nest) -> (Nest, Nest)
op [] (ys,sy)      =   (ys, sy)
op (x:xs) (ys,sy)  =   (ys ++ [Node x zs],sz ++ [Node x sy])
                         where (zs,sz) = op xs (sy, ys)
-}

-- queue
{-
data Queue a
type Forest a  =  Queue (Rose a)
data Rose a    =  Fork a (Forest a)

insert :: Queue a -> a -> Queue a
remove :: Queue a -> (a, Queue a)
empty  :: Queue a
isempty :: Queue a -> Bool

insert = undefined
remove = undefined
empty  = undefined
isempty = undefined

consQueue :: Queue a -> [Queue a] -> [Queue a]
consQueue xs xss  =   if isempty xs then xss else xs : xss

wrapQueue :: Queue a -> [Queue a]
wrapQueue xs = consQueue xs []

ncode              ::  Nest -> [Int]
ncode  =  unfoldr step . wrapQueue . fst . foldr op (empty,empty)

op (Node a xs) (bs,sb)
       =  (insert bs (Fork a cs),insert sc (Fork a sb))
          where (cs,sc) = foldr op (sb,bs) xs

step = undefined


box                 ::  [a] -> [a] -> [a]
xs `box` ys         =   mix xs (ys, reverse ys)

mix                 ::  [a] -> ([a],[a]) -> [a]
mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)
-}

--

box                 ::  [a] -> [a] -> [a]
xs `box` ys         =   mix xs (ys,reverse ys)

mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)

cox                 ::  [a] -> [a] -> [a]
xs `cox` ys         =   reverse (reverse xs `box` reverse ys)

type Nest    =  [Spider]
data Spider  =  Node Int [Leg]
data Leg     =  Dn Spider | Up Spider

boxall  ::  [[a]] -> [a]
boxall  =   foldr box []
coxall  ::  [[a]] -> [a]
coxall  =   foldr cox []

wcode, bcode  ::  [Leg] -> [Int]
wcode         =   coxall . map wc
bcode         =   boxall . map bc

wc                     ::  Leg -> [Int]
wc (Up (Node a legs))  =   wcode legs ++ [a] ++ bcode legs
wc (Dn (Node _ legs))  =   reverse (wcode legs)

bc                     ::  Leg -> [Int]
bc (Up (Node _ legs))  =   reverse (bcode legs)
bc (Dn (Node a legs))  =   wcode legs ++ [a] ++ bcode legs

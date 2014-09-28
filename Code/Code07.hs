module Code07 where

import Data.List (minimumBy)
import Data.Ord (comparing)

data Tree = Leaf Int | Fork Tree Tree
  deriving (Eq,Show)

fringe :: Tree -> [Int]
fringe (Leaf x)      =   [x]
fringe (Fork xt yt)  =   fringe xt ++ fringe yt

treeRec :: [Int] -> Tree
treeRec [x] = Leaf x
treeRec xs  = Fork (treeRec ys) (treeRec zs)
  where  n = length xs `div` 2
         (ys,zs) = splitAt n xs

treeIter :: [Int] -> Tree
treeIter = head . iter . map Leaf
  where 
    iter []  = []
    iter [t] = [t]
    iter (xt:yt:ts) = iter (Fork xt yt : iter ts)

-- First steps

cost             ::  Tree -> Int
cost (Leaf x)    =   x
cost (Fork u v)  =   1 + (cost u `max` cost v)

minBy0  ::  Ord a => (Tree -> a) -> [Tree] -> Tree
minBy0  =   minimumBy . comparing

mincostTree0  ::  [Int] -> Tree
mincostTree0  =   minBy0 cost . trees0

trees0         ::  [Int] -> [Tree]
trees0 [x]     =   [Leaf x]
trees0 (x:xs)  =   concatMap (prefixes x) (trees0 xs)

prefixes                 ::  Int -> Tree -> [Tree]
prefixes x t@(Leaf y)    =   [Fork (Leaf x) t]
prefixes x t@(Fork u v)  =   [Fork (Leaf x) t] 
                         ++  [Fork u' v | u' <- prefixes x u ]

--

foldrn             ::  (a -> b -> b) -> (a -> b) -> [a] -> b
foldrn f g [x]     =   g x
foldrn f g (x:xs)  =   f x (foldrn f g xs)


mincostTree1 :: [Int] -> Tree
mincostTree1 = minBy0 cost . trees1

trees1 :: [Int] -> [Tree]
trees1  =  foldrn (concatMap . prefixes) (wrap . Leaf)

wrap :: a -> [a]
wrap x = [x]

--

type Forest = [Tree]

mincostTree2 :: [Int] -> Tree
mincostTree2 = minBy0 cost . trees2

trees2 :: [Int] -> [Tree]
trees2 = map rollup . forests

forests  ::  [Int] -> [Forest]
forests  =   foldrn (concatMap . prefixes2) (wrap . wrap . Leaf)

prefixes2        ::  Int -> Forest -> [Forest]
prefixes2 x ts   =   [ Leaf x : rollup (take k ts) : drop k ts 
                     | k <- [1 .. length ts]]

rollup  ::  Forest -> Tree
rollup  =   foldl1 Fork

--

mincostTree3 :: [Int] -> Tree
mincostTree3  =   minBy3 cost . trees2

minBy3 :: Ord b => (a -> b) -> [a] -> a
minBy3 f      =   foldl1 (cmp3 f)

cmp3 :: Ord b => (a -> b) -> a -> a -> a
cmp3 f u v    =   if f u <= f v then u else v

--

-- cmp u v  = if u -<= v then u else v

-- Fusion
{-
h (foldrn f g xs)  =  foldrn f' g' xs
h (g x)   = g' x
h (f x y) = f' x (h y)

h (foldrn f g xs) ~> foldrn f' g' xs
-}

-- cost' = map cost . reverse . spine

insert' :: Int -> [Tree] -> [Tree]
insert' x ts = Leaf x : split' x ts

split'             ::  Int -> [Tree] -> [Tree]
split' x [u] = [u]
split' x (u:v:ts)  =   if x `max` cost u < cost v then u:v:ts
                       else split' x (Fork u v : ts)

-- Final Argorithm

mincostTree  ::  [Int] -> Tree
mincostTree  =   foldl1 Fork . map snd . foldrn insert (wrap . leaf)

insert       ::  Int -> [(Int, Tree)] -> [(Int, Tree)]
insert x ts  =   leaf x : split x ts

split             ::  Int -> [(Int, Tree)] -> [(Int, Tree)]
split x [u]       =   [u]
split x (u:v:ts)  =   if x `max` fst u < fst v then u:v:ts
                      else split x (fork u v : ts)

leaf              ::  Int -> (Int, Tree)
leaf x            =   (x, Leaf x)

fork              ::  (Int, Tree) -> (Int, Tree) -> (Int, Tree)
fork (a,u) (b,v)  =   (1 + (a `max` b), Fork u v)

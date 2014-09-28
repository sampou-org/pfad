{-# LANGUAGE NPlusKPatterns #-}
module Code26 where

type Node   =  Int
type Graph  =  Node -> (Node,Node)

left, right :: Graph -> Node -> Node
left  = (fst .)
right = (snd .)

setl, setr :: Graph -> Node -> Node -> Graph
setl g x y = \ z -> if x == z then (y,right g x) else g z
setr g x y = \ z -> if x == z then (left g x, y) else g z

mark0 :: Graph -> Node -> (Graph, Node -> Bool)
mark0 g root = seek0 (g,const False) [root]

seek0 :: (Graph, Node -> Bool) -> [Node] -> (Graph, Node -> Bool)
seek0 (g,m) [] = (g,m)
seek0 (g,m) (x:xs)
  |  not (m x)  =  seek0 (g,set m x) (left g x : right g x : xs)
  |  otherwise  =  seek0 (g,m) xs

set, unset  ::  (Node -> Bool) -> Node -> (Node -> Bool)
set f x     =   \ y  ->  if y == x  then True   else f y
unset f x   =   \ y  ->  if y == x  then False  else f y

-- Eliminating duplicate entries

mark1 :: Graph -> Node -> (Graph, Node -> Bool)
mark1 g root = seek1 (g,const False) root []

seek1 :: (Graph, Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek1 (g,m) x xs
  |  not (m x)  =  seek1 (g,set m x) (left g x) (x:xs)
  |  null xs    =  (g,m)
  |  otherwise  =  seek1 (g,m) (right g (head xs)) (tail xs)

-- Threading the stack

mark2         ::  Graph -> Node -> (Graph,Node -> Bool)
mark2 g root  =   seek2 (g,const False) (const False) root []

seek2 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> [Node] -> (Graph, Node -> Bool)
seek2 (g,m) p x xs
  |  not (m x)  =  seek2 (g,set m x) (set p x) (left g x) (x:xs)
  |  otherwise  =  find2 (g,m) p xs

find2 :: (Graph, Node -> Bool) -> (Node -> Bool) -> [Node] -> (Graph, Node -> Bool)
find2 (g,m) _ [] = (g,m)
find2 (g,m) p (y:ys)
  |  not (p  y)  =  find2 (g,m) p ys
  |  otherwise   =  seek2 (g,m) (unset p y) (right g y) (y:ys)

-- Representing the stack by a linked list

stack                      ::  Graph -> (Node -> Bool) -> Node -> [Node]
stack g p x  |  x == 0     =   []
             |  p x        =   x : stack g p (left g x)
             |  not (p x)  =   x : stack g p (right g x)

restore               ::  Graph -> (Node -> Bool) -> Node -> [Node] -> Graph
restore g _ _ []      =   g
restore g p x (y:ys)  |   p y        =  restore (setl g y x) p y ys
                      |   not (p y)  =  restore (setr g y x) p y ys

-- Schorr-Waite algorithm

mark :: Graph -> Node -> (Graph, Node -> Bool)
mark g root  =  seek3 (g,const False) (const False) root 0

seek3 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
seek3 (g,m) p x y
  |  not (m x)  =  seek3 (setl g x y,set m x) (set p x) (left g x) x
  |  otherwise  =  find3 (g,m) p x y

find3 :: (Graph, Node -> Bool) -> (Node -> Bool) -> Node -> Node -> (Graph, Node -> Bool)
find3 (g,m) p x y
  |  y == 0     =  (g,m)
  |  p y        =  seek3 (swing g y x,m) (unset p y) (right g y) y
  |  otherwise  =  find3 (setr g y x,m) p y (right g y)
     where  swing g y x  =  setr (setl g y x) y (left g y)

module Code17 where

import Data.List (tails,isPrefixOf)
import Prelude hiding (abs)

endswith0 :: Eq a => [a] -> [a] -> Bool
endswith0 ws xs = ws `elem` tails xs

matches0     ::  Eq a => [a] -> [a] -> [Int]
matches0 ws  =   map fst . filter (p . snd) . scanl step0 (0,e)

p = undefined
e = undefined
op = undefined

step0 :: (Int,[a]) -> a -> (Int,[a])
step0 (n,x) y = (n+1, op x y)

matches1 ws = map fst . filter (null . snd . snd) . scanl step1 (0,([],ws))

step1 (n,(us,vs)) x = (n+1,op1 (us,vs) x)

op1 (us,vs) x | [x] `isPrefixOf` vs = (us++[x],tail vs)
              | null us             = ([],ws)
              | otherwise           = op1 (split ws (tail us)) x
  where ws = us++vs

split ws xs = if xs `isPrefixOf` ws then (xs,ws `after` xs)
              else split ws (tail xs)

after []     _      = []
after xs     []     = xs
after (_:xs) (_:ys) = after xs ys

--

data Rep a = Null | Node a (Rep a) (Rep a)

abs (Node (us,vs) l r) = (us,vs)
rep (us,vs) = Node (us,vs) (left us vs) (right us vs)

left []     vs = Null
left (u:us) vs = rep (split ws us)
                   where ws = u:(us++vs)
right us []     = Null
right us (v:vs) = rep (us++[v],vs)

matchesMP ws = map fst . filter (ok . snd) . scanl step (0,root)
  where
    ok (Node vs l r)        =  null vs
    step (n,t) x            =  (n+1,op t x)
    op Null x               =  root
    op (Node [] l r) x      =  op l x
    op (Node (v:vs) l r) x  =  if v==x then r else op l x
    root                    =  grep Null ws
    grep l []               =  Node [] l Null
    grep l (v:vs)           =  Node (v:vs) l (grep (op l v) vs)

-- KMP

next Null              x  =  Null
next (Node [] l r)     x  =  Node [] l r
next (Node (v:vs) l r) x  =  if v == x then next l x
                             else Node (v:vs) l r

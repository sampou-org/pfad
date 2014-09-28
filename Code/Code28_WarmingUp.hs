module Code28_WarmingUp where

import Data.List (unfoldr)
import Prelude hiding (reverse,concat)
import qualified Prelude as P

loopless = unfoldr step . prolog

-- Four warm-up exercises

-- (1) id_L
id_L  ::  [a] -> [a]
id_L  =   unfoldr uncons . prolog

prolog :: [a] -> [a]
prolog = id

uncons         ::  [a] -> Maybe (a, [a])
uncons []      =   Nothing
uncons (x:xs)  =   Just (x,xs)

-- (2) reverse
reverse  ::  [a] -> [a]
reverse  =   unfoldr uncons . foldl (flip (:)) []

-- (3) concat
concat    ::  [[a]] -> [a]
concat    =   unfoldr step . filter (not . null)
  where
    step :: [[a]] -> Maybe (a,[[a]])
    step []            =   Nothing
    step ((x:xs):xss)  =   Just (x,consList xs xss)

consList   ::  [a] -> [[a]] -> [[a]]
consList xs xss  =  if null xs then xss else xs:xss

-- (4) preorder
type Forest a  =  [Rose a]
data Rose a    =  Node a (Forest a)

preorder  ::  Forest a -> [a]
preorder  =   unfoldr step . wrapList
  where
    step                       ::  [Forest a] -> Maybe (a,[Forest a])
    step []                    =   Nothing
    step ((Node x xs:ys):zss)  =   Just (x,consList xs (consList ys zss))

wrapList :: [a] -> [[a]]
wrapList xs = consList xs []

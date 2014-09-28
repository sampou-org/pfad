{-# LANGUAGE NoMonomorphismRestriction #-}

module Code14 where

import Data.List
import Data.Ord

maxtail0 :: Ord a => [a] -> [a]
maxtail0 = maximum . tails

op0 :: Ord a => [a] -> a -> [a]
op0 ys x = maximum [zs ++ [x] | zs <- tails ys, zs `isPrefixOf` ys]

--

borders0     ::  Ord a => [a] -> [[a]]
borders0 xs  =   [ ys | ys <- tails xs, ys `isPrefixOf` xs ]

borders1 :: Ord a => [a] -> [[a]]
borders1 []  =   [[]]
borders1 xs  =   xs : borders1 (border1 xs)

border1 :: Ord a => [a] -> [a]
border1 xs = maximumBy (comparing length) [ b | b <- tls, b `elem` hds ] -- specification
  where
    tls = tail $ tails xs
    hds = init $ inits xs

op1 :: Ord a => [a] -> a -> [a]
op1 ys x  =  maximum [zs ++ [x] | zs <- borders1 ys]

after :: Eq a => [a] -> [a] -> [a]
xs     `after` []  =  xs
(x:xs) `after` (y:ys)
  | x == y         = xs `after` ys
_      `after` _   = error "second operand is not prefix of first"

op2                                     ::  Ord a => [a] -> a -> [a]
op2 ys x  |  null ys                    =   [x]
          |  head (ys `after` zs) >= x  =   ys ++ [x]
          |  otherwise                  =   op2 zs x
             where  zs  =  border2 ys

border2                                        ::  Ord a => [a] -> [a]
border2 [_]  =  []
border2 ysx@(_:_) | head (ys `after` zs) <  x  =   border2 (zs ++ [x])
                  | head (ys `after` zs) == x  =   zs ++ [x]
                  | head (ys `after` zs) >  x  =   []
                    where ys = init ysx
                          x  = last ysx
                          zs = border2 ys
border2 _ = error "border2 is not defined on empty list."

cocktail2 :: Ord a => [a] -> ([a],[a])
cocktail2 xs
  = if null xs then ([],[])
    else (border2 (maxtail2 xs), maxtail2 xs `after` border2 (maxtail2 xs))

maxtail2 :: Ord a => [a] -> [a]
maxtail2 = uncurry (++) . cocktail2

--

maxtail3 :: Ord a => [a] -> [a]
maxtail3  = uncurry (++) . cocktail3

cocktail3 :: Ord a => [a] -> ([a], [a])
cocktail3 = foldl op3 ([],[])

op3 :: Ord a => ([a], [a]) -> a -> ([a], [a])
{-
op3 (zs,ws) x | null ws  =  ([],[x])
              | w <  x   =  cocktail3 (zs ++ [x])
              | w == x   =  (zs++[x],tail ws ++ [x])
              | w >  x   =  ([],zs++ws++[x])
                where  w = head ws
-}
op3 (_,[]) x            =  ([],[x])
op3 (zs,ws@(w:_)) x
              | w <  x   =  cocktail3 (zs ++ [x])
              | w == x   =  (zs++[x],tail ws ++ [x])
              | w >  x   =  ([],zs++ws++[x])

--

maxtail4 :: Ord a => [a] -> [a]
maxtail4  = thd . cocktail4

cocktail4 :: Ord a => [a] -> (Int,Int,[a],[a])
cocktail4 = foldl op4 (0,0,[],[])

op4 :: Ord a => (Int,Int,[a],[a]) -> a -> (Int,Int,[a],[a])
op4 (p,q,ys,ws) x | q == 0    =  (0,1,[x],[x])
                  | w <  x    =  cocktail4 (drop (q-r) ws ++ [x])
                  | w == x    =  (p+1,q,ys++[x],tail ws ++ [x])
                  | otherwise =  (0,p+q+1,ys++[x],ys++[x])
                    where  w = head ws
                           r = p `mod` q

thd :: (a,b,c,d) -> c
thd (_,_,z,_) = z

maxtail :: Ord a => [a] -> [a]
maxtail []          = []
maxtail xxs@(_:xs)  = step (0,1,xxs,xxs,xs)

step :: Ord a => (Int,Int,[a],[a],[a]) -> [a]
step (p,q,ys,ws,[]) = ys
step (p,q,ys,wws@(w:ws),xxs@(x:xs))
  | w <  x  = maxtail (drop (q-r) wws)
  | w == x  = step (p+1,q,ys,ws,xs)
  | w >  x  = step (0,p+q+1,ys,ys,xs)
    where  r = p `mod` q

--

sample0 :: String
sample0 = "7412741274"
sample1 :: String
sample1 = "mammam"

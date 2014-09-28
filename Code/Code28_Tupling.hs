{-# OPTIONS_GHC -cpp #-}
module Code28_Tupling where

(□)          ::  [a] -> [a] -> [a]
xs □ ys      =   mix xs (ys, reverse ys)

mix                 ::  [a] -> ([a],[a]) -> [a]
mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)

boxall  ::  [[a]] -> [a]
boxall  =   foldr (□) []

op             ::  [a] -> ([a], [a]) -> ([a], [a])
op xs (ys,sy)  =   (xs □ ys, xs ⊠ sy)

(⊠)      ::  [a] -> [a] -> [a]
#ifdef SPEC_OF_BOXTIMES
xs ⊠ sy  =   reverse (xs □ (reverse sy))
#else
[]      ⊠ sy  =   sy
(x:xs)  ⊠ sy  =   (xs ⊠ (reverse sy)) ++ [x] ++ sy
#endif

op1                  ::  [a] -> ([a], [a]) -> ([a], [a])
op1 []      (ys,sy)  =   (ys,sy)
op1 (x:xs)  (ys,sy)  =   (ys ++ [x] ++ zs,sz ++ [x] ++ sy)
                         where (zs,sz)  = op1 xs (sy,ys)

op2             ::  [a] -> ([a], [a]) -> ([a], [a])
op2 xs (ys,sy)  =   if even (length xs)
                    then (mix xs (ys,sy), mix (reverse xs) (sy,ys))
                    else (mix xs (ys,sy), mix (reverse xs) (ys,sy))

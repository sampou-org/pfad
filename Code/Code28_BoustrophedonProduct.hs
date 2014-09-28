{-# OPTIONS_GHC -cpp #-}
module Code28_BoustropehedonProduct where

-- Boustrophedon product

(□)          ::  [a] -> [a] -> [a]
#ifdef SPEC_OF_BOX
[] □ ys      =   ys
(x:xs) □ ys  =   ys ++ [x] ++ (xs □ reverse ys)
#else
xs □ ys           =   mix xs (ys, reverse ys)

mix                 ::  [a] -> ([a],[a]) -> [a]
mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)
#endif

boxall  ::  [[a]] -> [a]
boxall  =   foldr (□) []

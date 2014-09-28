module Code28 (module Code28_TreeAndQueue,(□)) where

import Code28_TreeAndQueue

(□)          ::  [a] -> [a] -> [a]
xs □ ys      =   mix xs (ys, reverse ys)

mix                 ::  [a] -> ([a],[a]) -> [a]
mix [] (ys,_)       =   ys
mix (x:xs) (ys,sy)  =   ys ++ [x] ++ mix xs (sy,ys)
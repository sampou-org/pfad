module Code30_Parity where

import Code30_Loopless hiding (bop,wop)

data Spider'  =  Node' (Bool,Bool) Int [Leg']
data Leg'     =  Dn' Spider' | Up' Spider'

decorate                ::  Spider -> Spider'
decorate (Node a legs)  =   node' a (map (mapLeg decorate) legs)

mapLeg                  ::  (Spider -> Spider') -> Leg -> Leg'
mapLeg f (Up x)         =   Up' (f x)
mapLeg f (Dn x)         =   Dn' (f x)

node' :: Int -> [Leg'] -> Spider'
node' a legs  =   Node' (foldr op (True,True) legs) a legs

op :: Leg' -> (Bool, Bool) -> (Bool, Bool)
op (Up' (Node' (w,b) _ _)) (w',b')  =   (w /= b && w', b && b')
op (Dn' (Node' (w,b) _ _)) (w',b')  =   (w && w', w /= b && b')

bop,wop  ::  Leg' -> [Int] -> [Int]
bop  (Up' (Node' (_,b) _ legs)) cs
  =  reverse (foldr bop (revif b cs) legs)
bop  (Dn' (Node' (w,_) a legs)) cs
  =  foldr wop (revif (not w ) cs) legs ++ [a] ++ foldr bop (revif w cs) legs
wop  (Up' (Node' (_,b) a legs)) cs
  =  foldr wop (revif b cs) legs ++ [a] ++ foldr bop (revif (not b) cs) legs
wop  (Dn' (Node' (w,_) _ legs)) cs
  =  reverse (foldr wop (revif w cs) legs)

revif :: Bool -> [a] -> [a]
revif b cs = if b then reverse cs else cs
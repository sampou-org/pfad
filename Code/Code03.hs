module Code03 where

type Nat = Integer

-- Jack

invertJ      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertJ f z  =   [ (x,y) | x <- [0..z], y <- [0..z], f (x,y) == z ]

-- Theo

invertT0      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertT0 f z  =   [ (x,y) | x <- [0 .. z], y <- [0 .. z-x], f(x,y) == z ]

invertT0'      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertT0' f z  =   [ (x,y) | x <- [0 .. w], y <- [0 .. w-x], f(x,y) == z ]
                   where  w =  z - f (0,0)

-- Anne

invertA      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertA f z  =   findA (0,z) f z

findA               ::  (Nat,Nat) -> ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
findA (u,v) f z
  | u > z || v < 0  =   []
  | z' <   z        =   findA (u+1,v) f z
  | z' ==  z        =   (u,v) : findA (u+1,v-1) f z
  | z' >   z        =   findA (u,v-1) f z
    where 
      z' = f (u,v)

-- Theo

invertT'      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertT' f z  =   findT' (0,m) f z
  where
    m = bsearch (\ y -> f (0,y)) (-1,z+1) z

findT'               ::  (Nat,Nat) -> ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
findT' (u,v) f z
  | u > n || v < 0  =  []
  | z' <   z        =  findT' (u+1,v) f z
  | z' ==  z        =  (u,v):findT' (u+1,v-1) f z
  | z' >   z        =  findT' (u,v-1) f z
    where 
      z'  = f (u,v)
      n   = bsearch (\ x -> f (x,0)) (-1,z+1) z

invertT      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertT f z  =   findT (0,m) (n,0) f z
  where
    m  =  bsearch (\ y -> f (0,y)) (-1,z+1) z
    n  =  bsearch (\ x -> f (x,0)) (-1,z+1) z

findT               ::  (Nat,Nat) -> (Nat,Nat) -> ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
findT (u,v) (r,s) f z
  | u > r || v < 0  =  []
  | z' <   z        =  findT (u+1,v) (r,s) f z
  | z' ==  z        =  (u,v) : findT (u+1,v-1) (r,s) f z
  | z' >   z        =  findT (u,v-1) (r,s) f z
    where 
      z'  = f (u,v)

bsearch :: (Nat -> Nat) -> (Nat,Nat) -> Nat -> Nat
bsearch g (a,b) z
  | a+1  == b  = a
  | g m  <= z  = bsearch g (m,b) z
  | otherwise  = bsearch g (a,m) z
    where  m = (a+b) `div` 2

-- Mary

invertM      ::  ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
invertM f z  =   findM (0,m) (n,0) f z
  where
    m  =  bsearch (\ y -> f (0,y)) (-1,z+1) z
    n  =  bsearch (\ x -> f (x,0)) (-1,z+1) z

findM               ::  (Nat,Nat) -> (Nat,Nat) -> ((Nat,Nat) -> Nat) -> Nat -> [(Nat,Nat)]
findM (u,v) (r,s) f z
  | u > r || v < s  = []
  | v - s <= r - u  = rfind (bsearch (\ x -> f (x,q)) (u-1,r+1) z)
  | otherwise       = cfind (bsearch (\ y -> f (p,y)) (s-1,v+1) z)
    where
      p        =  (u+r) `div` 2
      q        =  (v+s) `div` 2
      rfind p  =  (  if f (p,q) == z then (p,q) : findM (u,v) (p-1,q+1) f z
                     else findM (u,v) (p,q+1) f z) ++ 
                     findM (p+1,q-1) (r,s) f z
      cfind q  =  findM (u,v) (p-1,q+1) f z ++
                  (  if f (p,q) == z then (p,q) : findM (p+1,q-1) (r,s) f z
                     else findM (p+1,q) (r,s) f z)

f0,f1,f2,f3,f4 :: (Nat,Nat) -> Nat

f0 (x,y) = 2^y * (2*x + 1) + 1
f1 (x,y) = x*2^x + y*2^y + 2*x + y
f2 (x,y) = 3*x + 27*y + y^2
f3 (x,y) = x^2 + y^2 + x + y
f4 (x,y) = x + 2^y + y - 1

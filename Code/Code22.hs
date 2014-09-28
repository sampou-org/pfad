module Code22 where

import Data.List

-- School-book method

det1        ::  [[Integer]] -> Integer
det1 [[x]]  =   x
det1 xss    =   foldr1 (-) (zipWith (*) col1 (map det1 (minors cols)))
                where  col1  =  map head xss
                       cols  =  map tail xss

minors         ::  [a] -> [[a]]
minors []      =   []
minors (x:xs)  =   xs : map (x:) (minors xs)

-- Using rational division

det2        ::  [[Rational]] -> Rational
det2 [[x]]  =   x
det2 xss    =
  case break ((/= 0) . head) xss of
    (_,[])        -> 0
    (yss,zs:zss)  -> let x = head zs * det2 (reduce zs (yss++zss))
                     in if even (length yss) then x else -x

reduce :: [Rational] -> [[Rational]] -> [[Rational]]
reduce xs yss          =  map (reduce1 xs) yss

reduce1 :: [Rational] -> [Rational] -> [Rational]
reduce1 (x:xs) (y:ys)  =  zipWith (\ a b -> b - d*a) xs ys
                          where  d = y/x

-- Using integer division
det3        ::  [[Integer]] -> Integer
det3 [[x]]  =   x
det3 xss    =
  case break ((/= 0) . head) xss of
    (_,[])        -> 0
    (yss,zs:zss)  -> let  x  =  det3 (condense (zs:yss ++ zss))
                          d  =  head zs ^ (length xss - 2)
                          y  =  x `div` d
                     in   if even (length yss) then y else -y

condense :: [[Integer]] -> [[Integer]]
condense  =  map (map det . pair . uncurry zip) . pair
             where  pair (x:xs)       = map ((,) x) xs
                    det ((a,b),(c,d)) = a*d - b*c

-- Interleaving

det4  ::  [[Integer]] -> Integer
det4  =   det' 1

det'          ::  Integer -> [[Integer]] -> Integer
det' _ [[x]]  =   x
det' k xss    =
  case break ((/= 0) . head) xss of
    (_,[])        ->  0
    (yss,zs:zss)  ->  let x = det' (head zs) (cd k (zs:yss ++ zss))
                      in if even (length yss) then x else -x

cd :: Integer -> [[Integer]] -> [[Integer]]
cd k  =  map (map det . pair . uncurry zip) . pair
         where  pair (x:xs)        =  map ((,) x) xs
                det ((a,b),(c,d))  = (a * d - b * c) `div` k

-- No division using

det5      ::  [[Integer]] -> Integer
det5 ass  =   head (head bss)
  where
    bss   =   foldl (matmult . mut) ass' (replicate (n-1) ass)
    ass'  =   if odd n then ass else map (map negate) ass
    n     =   length ass

mut :: [[Integer]] -> [[Integer]]
mut xss =  zipWith (++) zeros (zipWith (:) ys (zipWith drop [1..] xss))
           where  ys = map negate (tail (scanr (+) 0 (diagonal xss)))

zeros :: [[Integer]]
zeros = [take j (repeat 0) | j <- [0..]]

diagonal :: [[a]] -> [a]
diagonal []        =  []
diagonal (xs:xss)  =  head xs : diagonal (map tail xss)

matmult :: Num a => [[a]] -> [[a]] -> [[a]]
matmult xss yss  =  zipWith (map . dp) xss (repeat (transpose yss))

dp :: Num a => [a] -> [a] -> a
dp xs ys  =  sum (zipWith (*) xs ys)

-- improved version

trimult :: Num a => [[a]] -> [[a]] -> [[a]]
trimult xss yss = zipWith (map . dp) xss (submats (transpose yss))

submats        ::  [[a]] -> [[[a]]]
submats [[x]]  =   [[[x]]]
submats xss    =   xss : submats (map tail (tail xss))

mut' :: Num a => [[a]] -> [[a]]
mut' xss  =  zipWith (:) ys (map tail xss)
  where  ys  =  map negate (tail (scanr (+) 0 (map head xss)))

det6      ::  [[Integer]] -> Integer
det6 ass  =   head (head bss)
  where
    bss   =   foldl (trimult . mut) ass' (replicate (n - 1) ass)
    ass'  =   if odd n then upper ass
              else map (map negate) (upper ass)
    n     =   length ass

upper  ::  [[a]] -> [[a]]
upper  =   zipWith drop [0..]

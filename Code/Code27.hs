{-# LANGUAGE NPlusKPatterns #-}
module Code27 where

type Array a = [(Int,a)]


-- insertAll n  =   scanl (insert n) [] . label n

specials    ::  Int -> [Int]
specials n  =   scanl1 (+) (halves n)

halves      ::  Int -> [Int]
halves n    =   if n == 1 then [] else m : halves (n-m)
                where m = n `div` 2

label       ::  Int -> [a] -> [(Int,a)]
label n xs  =   replace 1 (zip [1..] xs) (specials n)

replace :: Int -> [(Int, a)] -> [Int] -> [(Int, a)]
replace _ [] _ = []
replace i ((k,x):kxs) ns
  |  null  ns  =  [(0,x)]
  |  k <   n   =  (i,x) : replace i kxs ns
  |  k ==  n   =  (0,x) : replace (i+1) kxs (tail ns)
     where  n = head ns

insert n as (i,x)  =   if i == 0
                       then relocate (0,n) x as
                       else relocate (l,r) x as
                       where  (l,r) = ipick n as (i,x)

relocate             ::  Ord a => (Int,Int) -> a -> Array a -> Array a
relocate (l,r) x as  =   distribute (add x (entries (l,r) as)) (l,r) as

entries (l,r) as  =   [x | (i,x) <- as, l <= i && i < r]
add x xs          =   takeWhile (<x) xs ++ [x] ++ dropWhile (<x) xs

distribute              ::  [a] -> (Int,Int) -> Array a -> Array a
distribute xs (l,r) as  =   takeWhile (\(i,x) -> i < l) as ++
                            spread xs (l,r) ++
                            dropWhile (\(i,x) -> i < r) as

spread           ::  [a] -> (Int,Int) -> Array a
spread xs (l,r)  |   null  xs  =  []
                 |   n ==  0   =  [(m,head xs)]
                 |   n >   0   =  spread ys (l,m) ++ spread zs (m,r)
                     where  (n,m)    =  (length xs `div` 2,(l+r) `div` 2)
                            (ys,zs)  =  splitAt n xs

ipick             ::  Ord a => Int -> Array a -> (Int,a) -> (Int,Int)
ipick n as (i,x)  =   if p < q then (p,q) else
                      head [(l,r) |  (j,(l,r)) <- zip [0..] (ipath n p),
                                     let s = length (entries (l,r) as),
                                     densityTest i j n s (r-l)]
                      where  (p,q)  =  ipoint n x as


ipoint         ::  Ord a => Int -> a -> Array a -> (Int,Int)
ipoint n x as  =   search (0,n) as
  where  search (p,q) []            =  (p,q)
         search (p,q) ((i,y) : as)  =  if x < y then (p,i)
                                       else search (i+1,q) as

ipath n p  =   reverse (intervals (0,n) p)

intervals (l,r) p  |   l+1 == r  =  [(l,r)]
                   |   p < m     =  (l,r) : intervals (l,m) p
                   |   m <= p    =  (l,r) : intervals (m,r) p
                       where  m = (l+r) `div` 2

densityTest i' j' n s' w'  =   2^(i*j)*s^k < (2^i-1)^j*w^k
  where  (i,j,s,w)  =  convert toInteger (i',j',s',w')
         k          =  toInteger (ceiling (logBase 2 (fromIntegral n)))

convert f (a,b,c,d) = (f a,f b,f c,f d)
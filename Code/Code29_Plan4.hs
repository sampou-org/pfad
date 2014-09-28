module Code29 where

import Code28

boxall  ::  [[a]] -> [a]
boxall  =   boxall1

jcode    ::  Int -> [Int]
jcode  =   boxall . map bumpDn . pairs
           where  pairs n = addpair (0,n) []

bumpBy             ::  Int -> [Int] -> [Int]
bumpBy _ []        =   []
bumpBy k [a]       =   [a+k]
bumpBy k (a:b:as)  =   (a+k) : b : bumpBy k as

bumpDn        ::  (Int, Int) -> [Int]
bumpDn (k,n)  =   bumpBy k [n-1,n-2 .. 1]

addpair           ::  (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
addpair (_,1) ps  =   ps
addpair (k,n) ps  =   addpair (k',n-1) ((k,n):ps)
                      where  k' = if odd n then k+1 else 1

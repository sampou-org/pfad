module Code29_Plan1 where

import Code28

jcode    ::  Int -> [Int]
jcode n  =   code (0,n)

bumpBy             ::  Int -> [Int] -> [Int]
bumpBy _ []        =   []
bumpBy k [a]       =   [a+k]
bumpBy k (a:b:as)  =   (a+k) : b : bumpBy k as

bumpDn        ::  (Int, Int) -> [Int]
bumpDn (k,n)  =   bumpBy k [n-1,n-2 .. 1]

code        ::  (Int, Int) -> [Int]
code (_,1)  =   []
code (k,n)  =   code (k',n-1) □ bumpDn (k,n)
                where  k' = if odd n then k+1 else 1

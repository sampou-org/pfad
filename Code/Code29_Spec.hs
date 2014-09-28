module Code29_Spec where

import Code28

jcode    ::  Int -> [Int]
jcode 1  =   []
jcode n  =   (bumpBy 1 (jcode (n-1))) □ [n-1,n-2 .. 1]

bumpBy             ::  Int -> [Int] -> [Int]
bumpBy _ []        =   []
bumpBy k [a]       =   [a+k]
bumpBy k (a:b:as)  =   (a+k) : b : bumpBy k as

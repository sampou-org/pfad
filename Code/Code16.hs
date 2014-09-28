module Code16 where

import Data.Array (accumArray,(!))
import Data.List (inits,isSuffixOf,isPrefixOf)
import Code15

matches0     ::  Eq a => [a] -> [a] -> [Int]
matches0 ws  =   map length . filter (endswith0 ws) . inits

endswith0 :: Eq a => [a] -> [a] -> Bool
endswith0 = isSuffixOf

{- scan lemma
  map (foldl op e) . inits = scanl op e
-}

matches1     ::  Eq a => [a] -> [a] -> [Int]
matches1 ws  =   map fst . filter ((sw `isPrefixOf`) . snd) . scanl step (0,[])
                 where sw = reverse ws

step :: (Int,[a]) -> a -> (Int,[a])
step (n,sx) x = (n+1, x:sx)


matches ws  = test m . scanl step (0,[])
  where
    test j [] = []
    test j ((n,sx):nxs) 
      | i == m    = n : test k (drop (k-1) nxs)
      | m-k <= i  = test k (drop (k-1) nxs)
      | otherwise = test m (drop (k-1) nxs)
          where 
            i'  = llcp sw (take j sx)
            i   = if i'== j then m else i'
            k   = a ! i
    a = accumArray min m (0,m) (vks ++ vks')
    sw = reverse ws
    m  = length sw
    vks  = zip (allcp' sw) [1..m]
    vks' = zip [m,m-1..1] (foldr op [] vks)
    op (v,k) ks = if v+k==m then k:ks else head ks:ks

llcp :: Eq a => [a] -> [a] -> Int
llcp = llcp1

allcp' xs = tail (allcp xs) ++ [0]

spat = "aabaaabaa"
srpat = reverse spat
stxt = "aaababcabcabcc"
srtxt = reverse stxt

f k = llcp srpat (drop k srpat)
op (v,k) ks = if v+k==9 then k:ks else head ks:ks
vks = [(f k,k)|k <- [1..9]]
vks' = zip [9,8..1] (foldr op [] vks)

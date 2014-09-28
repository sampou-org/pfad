module Code30_TreeSpider where

import Code28 (boxall1)

boxall :: [[a]] -> [a]
boxall = boxall1

type Nest    =  [Spider]
data Spider  =  Node Int Nest

ncode              ::  Nest -> [Int]
ncode              =   boxall . map scode

scode              ::  Spider -> [Int]
scode (Node a xs)  =   a : ncode xs

module Code19 where

import Data.List ((\\))

(/\),(\/) :: Bool -> Bool -> Bool
(/\) = (&&)
(\/) = (||)

type Matrix a  =  [Row a]
type Row a     =  [a]

type Grid   =  Matrix Digit
type Digit  =  Char

digits  =  ['1'..'9']
blank   =  (== '0')

solve0 = filter valid . expand . choices

type Choices = [Digit]

choices   ::  Grid -> Matrix Choices
choices   =   map (map choice)
choice d  =   if blank d then digits else [d]

expand  ::  Matrix Choices -> [Grid]
expand  =   cp . map cp

cp           ::  [[a]] -> [[a]]
cp []        =   [[]]
cp (xs:xss)  =   [x:ys | x <- xs, ys <- cp xss]

valid    ::  Grid -> Bool
valid g  =   all nodups (rows g) 
         /\  all nodups (cols g)
         /\  all nodups (boxs g)

nodups         ::  Eq a => [a] -> Bool
nodups []      =   True
nodups (x:xs)  =   all (/= x) xs /\ nodups xs

rows  ::  Matrix a -> Matrix a
rows  =   id

cols           ::  Matrix a -> Matrix a
cols [xs]      =   [[x] | x <- xs]
cols (xs:xss)  =   zipWith (:) xs (cols xss)

boxs  ::  Matrix a -> Matrix a
boxs  =   map ungroup . ungroup . map cols . group . map group

group     ::  [a] -> [[a]]
group []  =   []
group xs  =   take 3 xs : group (drop 3 xs)

ungroup  ::  [[a]] -> [a]
ungroup  =   concat

pruneRow      ::  Row Choices -> Row Choices
pruneRow row  =   map (remove fixed) row
                  where  fixed = [ d | [d] <- row ]

remove        ::  Eq a => [a] -> [a] -> [a]
remove xs ds  =   if single ds then ds else ds \\ xs

single      ::  [a] -> Bool
single [_]  =   True
single _    =   False

prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows


pruneBy    ::  ([Row Choices] -> [Row Choices]) -> [Row Choices] -> [Row Choices]
pruneBy f  =   f . map pruneRow . f

solve1 :: Grid -> [Grid]
solve1 = filter valid . expand . prune . choices

expand1       ::  Matrix Choices -> [Matrix Choices]
expand1 rows  =   [rows1 ++ [row1++[c]:row2]++rows2 | c <- cs]
  where  (rows1,row:rows2)  = break (any smallest) rows
         (row1,cs:row2)     = break smallest row
         smallest cs        = length cs == n
         n                  = minimum (counts rows)

counts :: Matrix Choices -> [Int]
counts = filter (/= 1) . map length . concat

complete ::  Matrix Choices -> Bool
complete  =  all (all single)

safe :: Eq a => Matrix [a] -> Bool
safe m  =  all ok (rows m) /\ all ok (cols m) /\ all ok (boxs m)
  where  ok row = nodups [ d | [d] <- row ]


solve :: Grid -> [Grid]
solve     =   search . choices


search :: Matrix Choices -> [Grid]
search m  |   not (safe m)   =  []
          |   complete m'    =  [map (map head) m']
          |   otherwise      =  concat (map search (expand1 m'))
              where  m' = prune m

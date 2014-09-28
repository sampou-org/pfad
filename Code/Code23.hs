{-# LANGUAGE NPlusKPatterns #-}
module Code23 where

import Data.List ((\\),sort,transpose)
import Code22 (det4)

import Test.QuickCheck

det  ::  [[Integer]] -> Integer
det  =   det4

type Point = [Integer]

dimension     ::  Point -> Int
dimension ps  =   length ps - 1

type Simplex = ([Point],Int)

orientation  ::  [Point] -> Int
orientation  =   fromIntegral . signum . det

type Facet = ([Point],Int)

facets         ::  Simplex -> [Facet]
facets (us,b)  =   zip (minors us) (cycle [b,-b])

minors         ::  [a] -> [[a]]
minors []      =   []
minors (x:xs)  =   xs : map (x:) (minors xs)

insideCS        ::  Simplex -> Point -> Bool
insideCS smp p  =   and [ 0 <= b * orientation (p:us) | (us,b) <- facets smp ]

-- convex hull

insideCH       ::  [Point] -> Point -> Bool
insideCH vs p  =   or [ insideCS smp p | smp <- simplexes vs ]

simplexes     ::  [Point] -> [Simplex]
simplexes vs  =   [(us,b)  | us <- tuples (d+1) vs
                           , let b = orientation us, b /= 0]
                  where  d = dimension (head vs)

tuples           ::  Int -> [a] -> [[a]]
tuples 0 _       =   [[]]
tuples _ []      =   []
tuples n (x:xs)  =   map (x:) (tuples (n-1) xs) ++ tuples n xs

-- An incremental algorithm

insideCH'       ::  [Point] -> Point -> Bool
insideCH' vs p  =   or [insideCS smp p | smp <- partition vs]

partition :: [Point] -> [Simplex]
partition vs
  = case findSimplex0 vs of
      Nothing   ->  []
      Just smp  ->  foldl update [smp] (vs \\ vertices smp)

vertices  ::  Simplex -> [Point]
vertices  =   sort . fst

-- findSimplex

findSimplex0     ::  [Point] -> Maybe Simplex
findSimplex0 vs  =   if null smps then Nothing else Just (head smps)
                     where  smps = simplexes vs

degenerate        ::  Int -> [[Integer]] -> Bool
degenerate k      =   all (== 0) . map det . submatrices k . transpose

submatrices       ::  Int -> [a] -> [[a]]
submatrices k vs  =   map (++[last vs]) (tuples k (init vs))

findSimplex         ::  [Point] -> Maybe Simplex
findSimplex []      =   Nothing
findSimplex (v:vs)  =   search (dimension v) 1 [v] vs

search :: Int -> Int -> [Point] -> [[Integer]] -> Maybe Simplex
search d k us vs
  |  k == d + 1           =   Just (us, orientation us)
  |  null vs              =   Nothing
  |  degenerate k (v:vs)  =   search d k us (tail vs)
  |  otherwise            =   search d (k+1) (v:vs) (tail vs)
     where  v = head vs

-- update

external  ::  [Simplex] -> [Facet]
external  =   foldr op [] . sort . concatMap facets

op                  :: Simplex -> [Simplex] -> [Simplex]
op smp []           =   [smp]
op smp (smp':smps)  =   if vertices smp == vertices smp' then smps
                        else smp : smp' : smps

visible       ::  Point -> [Facet] -> [Facet]
visible v fs  =   [(us,b) | (us,b) <-fs, b*orientation (v:us) < 0]

newSimplex           ::  Point -> Facet -> Simplex
newSimplex v (us,b)  =   (v:us,-b)

update :: [Simplex] -> Point -> [Simplex]
update smps v
  =  smps ++ map (newSimplex v) (visible v (external smps))

-- An inprovement
faces     ::  [Point] -> [Facet]
faces vs  =   case findSimplex0 vs of
                Nothing   ->  []
                Just smp  ->  foldl update' (facets smp) (vs \\ vertices smp)

insideCH''Bad       ::  [Point] -> Point -> Bool
insideCH''Bad vs p  =   and [0 <= b*orientation (p:us) | (us,b) <- faces vs]

update'       ::  [Facet] -> [Integer] -> [Facet]
update' fs v  =   (fs \\ fs') ++ map (newFacet v) (external fs')
                  where  fs' = visible v fs

newFacet :: Point -> Facet -> Facet
newFacet v (us,b)  =  (v:us, b)

-- QuickCheck

point    ::  Int -> Gen [Integer]
point d  =   do { xs <- vector d; return (xs ++ [1])}

points          ::  Int -> Int -> Gen [[Integer]]
points _ 0      =   return []
points d (n+1)  =   do { p <- point d; ps <- points d n; return (p:ps)}

prop_Hull      ::  Int -> Int -> Property
prop_Hull d n  =   forAll (points d n) $ \ vs ->
                   forAll (point d) $ \ v ->
                   insideCH vs v == insideCH'' vs v

insideCH''       ::  [Point] -> Point -> Bool
insideCH'' vs v  =   if null fs then False
                     else and [0 <= b*orientation (v:us) | (us,b) <- fs]
                     where  fs = faces vs

--

pos1s = [[0,1],[1,1]]
pos2s = [[0,0,1],[1,0,1],[1,1,1]]

smp1,smp2 :: Simplex
smp1 =  (pos1s,orientation pos1s)
smp2 =  (pos2s,orientation pos2s)

p2In = [2,1,4]
p2Out = [1,2,2]
p2On  = [1,1,2]

strictIn p = and [ b == orientation (p:us) | (us,b) <- facets smp2 ]
strictOn p = or  [ 0 == orientation (p:us) | (us,b) <- facets smp2 ]

sqr :: [Point]
sqr = [[0,0,1],[1,0,1],[1,1,1],[0,1,1]]

cub :: [Point]
cub = [[0,0,0,1],[0,1,0,1],[1,1,0,1],[1,0,0,1]
      ,[0,0,1,1],[0,1,1,1],[1,1,1,1],[1,0,1,1]
      ]

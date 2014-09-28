module Code18 where

import Data.List

moves   :: State -> [Move]
move    :: State -> Move -> State
solved  :: State -> Bool

solve :: State -> Maybe [Move]
solve = undefined

type Path     = ([Move],State)
type Frontier = [Path]

bfsearch :: [State] -> Frontier -> Maybe [Move]
bfsearch _  [] = Nothing
bfsearch qs (p@(ms,q):ps)
  |  solved q     =  Just ms
  |  q `elem` qs  =  bfsearch qs ps
  |  otherwise    =  bfsearch (q:qs) (ps ++ succs p)

succs         ::  Path -> [Path]
succs (ms,q)  =   [(ms++[m], move q m) | m <- moves q]

{-
bfsearch' :: [State] -> [Frontier] -> Frontier -> Maybe [Move]
bfsearch' _  []   []  = Nothing
bfsearch' qs pss  []  = bfsearch' qs [] (concat (reverse pss))
bfsearch' qs pss  (p@(ms,q):ps)
  |  solved q     =  Just ms
  |  q `elem` qs  =  bfsearch' qs pss ps
  |  otherwise    =  bfsearch' (q:qs) (succs p : pss) ps
-}
bfsearch' :: [State] -> Frontier -> Frontier -> Maybe [Move]
bfsearch' _  []  []  = Nothing
bfsearch' qs rs  []  = bfsearch' qs [] rs
bfsearch' qs rs  (p@(ms,q):ps)
  |  solved q     =  Just ms
  |  q `elem` qs  =  bfsearch' qs rs ps
  |  otherwise    =  bfsearch' (q:qs) (succs p ++ rs) ps

bfsolve    ::  State -> Maybe [Move]
bfsolve q  =   bfsearch' [] [] [([],q)]

-- Plannig

type Plan = [Move]

premoves  ::  State -> Move -> [[Move]]

newplans       ::  State -> Plan -> [Plan]
{-
newplans q ms  =   mkplans ms
  where
    qms                =  moves q
    mkplans ms
      |  null ms       =  []
      |  m `elem` qms  =  [ms]
      |  otherwise     =  concat  [  mkplans (pms ++ ms)
                                  |  pms <- premoves q m
                                  ,  all (`notElem` ms) pms]
         where
           m    =  head ms
-}

type APath     =  ([Move],State,Plan)
type AFrontier  =  [APath]

psearch :: [State] -> AFrontier -> Maybe [Move]
psearch _  [] = Nothing
psearch qs (p@(ms,q,_):ps)
  |  solved q     =  Just ms
  |  q `elem` qs  =  psearch qs ps
  |  otherwise    =  psearch (q:qs) (asuccs p ++ ps ++ bsuccs p)

asuccs,bsuccs :: APath -> [APath]
asuccs (ms,q,plan)
  = [(ms++[m],move q m,plan') | m:plan' <- newplans q plan]
bsuccs (ms,q,_)
  = [(ms++[m],q',goalmoves q') | m <- moves q, let q' = move q m]

goalmoves  ::  State -> Plan

psearch' :: [State] -> AFrontier -> AFrontier -> Maybe [Move]
psearch' _  [] [] = Nothing
psearch' qs rs [] = psearch' qs [] rs
psearch' qs rs (p@(ms,q,_):ps)
  | solved q     =  Just ms
  | q `elem` qs  =  psearch' qs rs ps
  | otherwise    =  psearch' (q:qs) (bsuccs p++rs) (asuccs p++ps)

psolve :: State -> Maybe [Move]
psolve q  =   psearch' [] [] [([],q,goalmoves q)]

-- Rush Hour

type Cell     =  Int
type Grid     =  [(Cell,Cell)]
type Vehicle  =  Int  

type Move     =  (Vehicle,Cell)
type State    =  Grid

occupied  ::  Grid -> [Cell]
occupied  =   foldr (merge . fillcells) []

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xxs@(x:xs) yys@(y:ys)
  | x < y     = x : merge xs yys
  | x > y     = y : merge xxs ys
  | otherwise = x : merge xs ys

fillcells :: (Cell,Cell) -> [Cell]
fillcells (r,f) = if r > f - 7 then [r..f] else [r,r+7 .. f]

freecells :: Grid -> [Cell]
freecells g = allcells \\ occupied g

allcells :: [Cell]
allcells = [ c | c <- [1..41], c `mod` 7 /= 0 ]

-- moves  :: Grid -> [Move]
moves g     =   [(v,c) | (v,i) <- zip [0..] g, c <- adjs i, c `elem` fs]
                where  fs = freecells g

adjs        ::  (Cell,Cell) -> [Cell]
adjs (r,f)  =   if r > f - 7 then [f+1,r-1] else [f+7,r-7]

-- move :: Grid -> (Vehicle,Cell) -> Grid
move g (v,c)  =   g1 ++ adjust i c : g2
                  where  (g1,i:g2) = splitAt v g

adjust :: (Cell,Cell) -> Cell -> (Cell,Cell)
adjust (r,f) c
  | r > f -7   =  if c > f then (r+1,c) else (c,f-1)
  | otherwise  =  if c < r then (c,f-7) else (r+7,c)

-- solved    ::  Grid -> Bool
solved g  =   snd (head g) == 20


-- goalmoves  :: Grid -> Plan
goalmoves g  =  [ (0,c) | c <- [snd (head g)+1 ..20] ]

blocker      ::  Grid -> Cell -> (Vehicle,(Cell,Cell))
blocker g c  =   search (zip [0..] g) c

search :: [(Vehicle,(Cell,Cell))] -> Cell -> (Vehicle,(Cell,Cell))
search ((v,i):vis) c = if covers c i then (v,i) else search vis c

covers :: Cell -> (Cell,Cell) -> Bool
covers c (r,f) = r <= c && c <= f && (r > f-7 || (c-r)`mod`7 == 0)

freeingmoves :: Cell -> (Vehicle,(Cell,Cell)) -> [[Move]]
freeingmoves c (v,(r,f))
  | r > f - 7  =   [[(v,j)| j <- [f+1..c+n]] | c+n < k+7]
               ++  [[(v,j)| j <- [r-1,r-2..c-n]] | c-n > k]
  | otherwise  =   [[(v,j)| j <- [r-7,r-14..c-m]] | c-m > 0]
               ++  [[(v,j)| j <- [f+7,f+14..c+m]] | c+m < 42]
    where (k,m,n) = (f-f`mod`7,f-r+7,f-r+1)

-- premoves  ::  Grid -> Move -> [[Move]]
premoves g (v,c)  =  freeingmoves c (blocker g c)

-- newplans  ::  Grid -> Plan -> [Plan]
newplans g []     = []
newplans g (m:ms) = mkplans (expand g m ++ ms)
  where
    gms        =  moves g
    mkplans ms =  if m `elem` gms then [ms]
                  else concat [  mkplans (pms ++ ms) |
                                 pms <- premoves g m,
                                 all (`notElem` ms) pms]
                  where  m = head ms

expand :: Grid -> Move -> [Move]
expand g (v,c)
  |  r > f-7    =  if c > f then [(v,p) | p <- [f+1 .. c]]
                   else [(v,p) | p <- [r-1,r-2 .. c]]
  |  otherwise  =  if c > f then [(v,p) | p <- [f+7,f+14 .. c]]
                   else [(v,p) | p <- [r-7,r-14 .. c]]
     where  (r,f) = g !! v

--
puzzle0 :: Grid
puzzle0 = [(15,16),(17,24)] :: Grid
puzzle1,puzzle2,puzzle3,puzzle4 :: Grid
puzzle1 = [(17,18),(1,15),(2,9),(3,10),(4,11),(5,6),(12,19)
          ,(13,27),(24,26),(31,38),(33,34),(36,37),(40,41)]
puzzle2 = [(15,16),(1,8),(4,6),(9,10),(11,18),(17,24),(20,34)
          ,(31,38),(32,33),(39,41)]
puzzle3 = [(16,17),(1,15),(3,10),(4,6),(11,18),(22,23),(24,25)
          ,(27,41),(36,37),(38,39)]
puzzle4 = [(18,19),(1,15),(2,3),(5,12),(9,16),(10,17),(13,27)
          ,(22,24),(25,32),(31,38),(33,34),(36,37),(39,40)]

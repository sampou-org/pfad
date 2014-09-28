{-# LANGUAGE NPlusKPatterns #-}
module Code11 where

mnss0  ::  [Int] -> Int
mnss0  =   maximum . map sum . nonsegs

markings     ::  [a] -> [[(a,Bool)]]
markings xs  =   [zip xs bs | bs <- booleans (length xs)]

booleans        ::  Int -> [[Bool]]
booleans 0      =   [[]]
booleans (n+1)  =   [b:bs | b <- [True,False], bs <- booleans n]
booleans _      =   error "Negative argument"

nonsegs  ::  [a] -> [[a]]
nonsegs  =   extract . filter nonseg . markings

extract  ::  [[(a,Bool)]] -> [[a]]
extract  =   map (map fst . filter snd)

data State = E | S | M | N deriving (Eq)

nonseg :: [(a,Bool)] -> Bool
nonseg = (== N) . foldl step0 E . map snd

step0 :: State -> Bool -> State
step0 E False = E
step0 E True  = S
step0 S False = M
step0 S True  = S
step0 M False = M
step0 M True  = N
step0 N _     = N

-- Drivation

mnss1 :: [Int] -> Int
mnss1 = maximum . map sum . extract . filter nonseg . markings

pick    ::  State -> [a] -> [[a]]
pick q  =   extract . filter ((== q) . foldl step0 E . map snd) . markings

-- pickall xs = (pick E xs, pick S xs, pick M xs, pick N xs)

pickall :: [a] -> ([[a]],[[a]],[[a]],[[a]])
pickall = foldl step ([[]],[],[],[])

step :: ([[a]],[[a]],[[a]],[[a]]) -> a -> ([[a]],[[a]],[[a]],[[a]])
step (ess,sss,mss,nss) x = (ess
                           ,map (++[x]) (sss ++ ess)
                           ,mss ++ sss
                           ,nss ++ map (++[x]) (nss ++ mss))

mnss2 :: [Int] -> Int
mnss2 = maximum . map sum . fourth . pickall

fourth            ::  (a,b,c,d) -> d
fourth (_,_,_,z)  =   z

mnss3 :: [Int] -> Int
mnss3 = fourth . tuple (maximum . map sum) . pickall

tuple              ::  (a -> b) -> (a,a,a,a) -> (b,b,b,b)
tuple f (w,x,y,z)  =   (f w, f x, f y, f z)

-- final

mnss     ::  [Int] -> Int
mnss xs  =   fourth (foldl h (start (take 3 xs)) (drop 3 xs))
  where
    h (e,s,m,n) x =  (e
                     ,(s `max` e) + x
                     ,m `max` s
                     ,n `max` ((n `max` m) + x))

start :: [Int] -> (Int, Int, Int, Int)
start [x,y,z] = (0,maximum [x+y+z,y+z,z],maximum [x,x+y,y],x+z)
start _ = error "start: Length of list arg is not 3."

--

sample :: [Int]
sample = [-4,-3,-7,2,1,-2,-1,-4]

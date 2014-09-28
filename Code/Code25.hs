{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NPlusKPatterns #-}
module Code25 where

import Data.List (unfoldr,isPrefixOf)

-- New definitions

type Interval = (Int,Int)
type Fraction = Rational
type Bit = Int

d,e :: Int
d = undefined
e = undefined

data Symbol
data Model

interval  ::  Model -> Symbol -> Interval
symbol    ::  Model -> Int -> Symbol
interval  =   undefined
symbol    =   undefined

adapt  ::  Model -> Symbol -> Model
adapt  =   undefined

intervals           ::  Model -> [Symbol] -> [Interval]
intervals _ []      =   []
intervals m (x:xs)  =   interval m x : intervals (adapt m x) xs


(|>)            ::  Interval -> Interval -> Interval
(l,r) |> (p,q)  =   (l+floor (fromIntegral((r-l)*p)/2^d),l+floor (fromIntegral((r-l)*q)/2^d))

{-
bit :: Interval -> Maybe (Bit, Interval)
bit (l,r)  |  r <= 1/2   =  Just (0,(2*l,2*r))
           |  1/2 <= l   =  Just (1,(2*l-1,2*r-1))
           |  otherwise  =  Nothing
-}

ibit :: Interval -> Maybe (Bit,Interval)
ibit (l,r)  |  r <= 2^(e-1)  =  Just (0,(2*l,2*r))
            |  2^(e-1) <= l  =  Just (1,(2*l-2^e,2*r-2^e))
            |  otherwise     =  Nothing

encode1 :: Model -> [Symbol] -> [Bit]
encode1 m  =  unfoldr ibit . foldl (|>) (0,2^e) . intervals m

stream :: (a -> Maybe (b, a)) -> (a -> c -> a) -> a -> [c] -> [b]
stream f g s xs  =  unfoldr step (s,xs)
  where
    step (s,xs) = case f s of
                    Just (y,s')  ->  Just (y,(s',xs))
                    Nothing      ->  case xs of
                                       x:xs'  -> step (g s x,xs')
                                       []     -> Nothing

-- Incremental encoding and interval expansion

encode2 :: Model -> [Symbol] -> [Bit]
encode2 m  =  stream ibit (|>) (0,2^e) . intervals m

-- Interval expansion

-- widen :: Int -> Int -> Int
widen n x  =  2^n * (x-2^(e-1)) + 2^(e-1)

e1,e2,e3,e4 :: Int
e1 = 2^(e-2)
e2 = 2*e1
e3 = 3*e1
e4 = 4*e1

expand :: Interval -> (Int, Interval)
expand i = extend (0,i)

extend :: (Int,Interval) -> (Int,Interval)
extend (n,(l,r))
  |  e1 <= l && r <= e3  =  extend (n+1,(2*l-e2,2*r-e2))
  |  otherwise           =  (n,(l,r))

contract :: (Int,Interval) -> Interval
contract (n,(l,r)) = (shorten n l, shorten n r)

shorten :: Int -> Int -> Int
shorten n x = fromEnum $ toRational (fromIntegral (x-e2)/2^n + fromIntegral e2)

enarrow       ::  (Int,Interval) -> Interval -> (Int,Interval)
enarrow ei j  =   (n,i |> j)  where  (n,i) = extend ei

-- A New definition

-- encode3 :: Model -> [Symbol] -> [Bit]
-- encode3 m = stream ebit enarrow (0,(0,2^e)) . intervals m

{-
ebit :: (Int, Interval) -> Maybe (Bit,(Int,(Int,Int)))
ebit (0,(l,r))
  |  r <= e2   =  Just (0,(0,(2*l,2*r)))
  |  e2 <= l   =  Just (1,(0,(2*l-e4,2*r-e4)))
  |  otherwise  =  Nothing
ebit (n+1,(l,r))
  |  r <= e2   =  Just (0,(n,(l+2^n*e2,r+2^n*e2)))
  |  e2 <= l   =  Just (1,(n,(l-2^n*e2,r-2^n*e2)))
  |  otherwise  =  Nothing
-}

ebits :: (Int,Interval) -> Maybe ([Bit],(Int, Interval))
ebits (n,(l,r))
  |  r <= e2   =  Just (bits n 0,(0,(2*l,2*r)))
  |  e2 <= l   =  Just (bits n 1,(0,(2*l-e4,2*r-e4)))
  |  otherwise  =  Nothing

bits :: Int -> Bit -> [Bit]
bits n b = b : replicate n (1-b)

encode3 :: Model -> [Symbol] -> [Bit]
encode3 m = concat . stream ebits enarrow (0,(0,2^e)) . intervals m

-- Inverting streams

destream :: Eq a => (b -> Maybe (a,b)) -> (b -> c -> b) -> (b -> [a] -> c) -> b -> [a] -> [c]
destream f g h s ys   =  unfoldr step (s,ys)
  where  step (s,ys)  =  case f s of
                           Just (y,s')  ->  step (s',ys `after` [y])
                           Nothing      ->  Just (x,(g s x,ys))
                           where  x = h s ys

after :: Eq a => [a] -> [a] -> [a]
after usvs us | us `isPrefixOf` usvs = drop (length us) usvs

{-
decode m bs = unfoldr step (m,(0,(0,e4)),bs)
step (m,(n,(l,r)),bs)
  |  r <= e2   =  step (m,(0,(2*l,2*r)),bs `after` bits n 0)
  |  e2 <= l   =  step (m,(0,(2*l-e4,2*r-e4)),bs `after` bits n 1)
  |  otherwise  =  Just (x,(adapt m x,enarrow (n,(l,r)) (interval m x),bs))
  where  x = h (m,(n,(l,r))) bs
-}

(<|) :: Int -> Interval -> Int
k <| (l,r) = ((k-l+1)*2^d - 1) `div` (r-l)

h (m,ei) bs = symbol m (floor (widen n (2^e * toFrac bs)) <| i)
              where  (n,i) = extend ei

toFrac :: [Bit] -> Fraction
toFrac = foldr (\ b f -> (toRational b+f)/2) (1/2)

--

deocde :: Model -> [Bit] -> [Symbol]
deocde m bs  =  unfoldr step (m,(0,e4),toInt (take e bs'),drop e bs')
  where
    bs' = bs ++ 1:repeat 0

step :: (Model, Interval, Int, [Bit]) -> Maybe (Symbol, (Model, Interval, Int, [Bit]))
step (m,(l,r),n,b:bs)
      |  r <= e2             =  step (m,(2*l,2*r),2*n+b,bs)
      |  e2 <= l             =  step (m,(2*l-e4,2*r-e4),2*n-e4+b,bs)
      |  e1 <= l && r <= e3  =  step (m,(2*l-e2,2*r-e2),2*n-e2+b,bs)
      |  otherwise           =  Just (x,(adapt m x,(l,r) |> interval m x,n,b:bs))
     where  x = symbol m (n <| (l,r))

toInt :: [Bit] -> Int
toInt = foldl (\ n b -> 2*n + b) 0

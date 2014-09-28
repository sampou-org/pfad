{-# LANGUAGE EmptyDataDecls #-}
module Code24 where

import Debug.Trace
import Data.List (unfoldr,find)
import Data.Ratio

type Fraction = Ratio Integer
type Interval = (Fraction, Fraction)

(|>)                ::  Interval -> Interval -> Interval
(l1,r1) |> (l2,r2)  =   (l1+(r1-l1)*l2,l1+(r1-l1)*r2)


(<|) ::  Fraction -> Interval -> Fraction
f <| (l,r)  =   (f - l) / (r - l)

(<||) :: Interval -> Interval -> Interval
(l,r) <|| j = (l <| j, r <| j)

type Symbol = Char
type Model = [(Char,Interval)]

interval  ::  Model -> Symbol -> Interval
symbol    ::  Model -> Fraction -> Symbol
interval m s = maybe (error "Model does not include the symbol") id (lookup s m)
symbol m f = maybe (error "Model does not include the fraction") fst (find ((\(l,h) -> l <= f && f < h) . snd) m)

adapt  ::  Model -> Symbol -> Model
adapt m _ =  m

intervals           ::  Model -> [Symbol] -> [Interval]
intervals _ []      =   []
intervals m (x:xs)  =   interval m x : intervals (adapt m x) xs

-- Encoding

encode    ::  Model -> [Symbol] -> Fraction
encode m  =   pick . foldl (|>) (0,1) . intervals m

pick :: Interval -> Fraction

samplemd :: Model 
samplemd = [('e',(0,3/8)),('g',(3/8,1/2)),('n',(1/2,5/8)),('r',(5/8,7/8)),('v',(7/8,1))]

-- Decoding

decode        ::  Model -> Fraction -> [Symbol]
decode m f    =   unfoldr step (m,(0,1),f)
  where
    step (m,i,f)  =   Just (x,(adapt m x, i  |>  interval m x, f))
                      where  x = symbol m (f <| i)

-- Incremental encoding and decoding

type Bit = Fraction

toBits  ::  Interval -> [Bit]
toFrac  ::  [Bit] -> Fraction

encode'        ::  Model -> [Symbol] -> [Bit]
encode' m      =   toBits . foldl (|>) (0,1) . intervals m

decode'        ::  Model -> [Bit] -> [Symbol]
decode' m bs   =   unfoldr step (m,(0,1),toFrac bs)
  where
    step (m,i,f)  =   Just (x,(adapt m x, i  |>  interval m x, f))
                      where  x = symbol m (f <| i)

-- Streaming

stream :: (a -> Maybe (b, a)) -> (a -> c -> a) -> a -> [c] -> [b]
stream f g s xs  =  unfoldr step (s,xs)
  where
    step (s,xs) = case f s of
                    Just (y,s')  ->  Just (y,(s',xs))
                    Nothing      ->  case xs of
                                       x:xs'  -> step (g s x,xs')
                                       []     -> Nothing

encode'' m = stream bit (|>) (0,1) . intervals m

bit :: Interval -> Maybe (Bit, Interval)
bit (l,r)  |  r <= 1/2    =  Just (0,(2*l,2*r))
           |  1/2 <= l    =  Just (1,(2*l-1,2*r-1))
           |  otherwise   =  Nothing

toBits = unfoldr bit
toFrac = foldr (\ b f -> (b+f)/2) (1/2)

pick (l,r)  |  r <= 1/2   =  trace (show (l,r)) $ pick (2*l,2*r) / 2
            |  1/2 <= l   =  trace (show (l,r)) $ (1+pick (2*l-1,2*r-1)) / 2
            |  otherwise  =  trace (show (l,r)) $ 1/2


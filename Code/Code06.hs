{-# LANGUAGE EmptyDataDecls #-}
module Code06 where

import Data.List

type Data = [Datum]

-- Specification

candidates  ::  Data -> [Candidate]
value       ::  Candidate -> Value
good        ::  Value -> Bool
solutions   ::  Data -> [Candidate]

solutions0   ::  Data -> [Candidate]
solutions0   =   filter (good . value) . candidates0
candidates0  ::  Data -> [Candidate]
candidates0  =   foldr extend []

extend   ::  Datum -> [Candidate] -> [Candidate]
extend'  ::  Datum -> [Candidate] -> [Candidate]
extend' x = filter (ok . value) . extend x

ok  ::  Value -> Bool
ok  =   undefined

candidates1 :: Data -> [(Candidate, Value)]
candidates1 = map (fork (id, value)) . foldr extend' []

-- Utility

fork :: (a -> b, a -> c) -> a -> (b,c)
fork (f,g) x = (f x, g x)

cross :: (a -> b, c -> d) -> (a,c) -> (b,d)
cross (f,g) (x,y) = (f x,g y)

zipp  ::  ([a],[b]) -> [(a,b)]
zipp  =   uncurry zip

--
solutions1 :: Data -> [Candidate]
solutions1 = map fst . filter (good . snd) . foldr expand1 []
expand1 :: Datum -> [(Candidate, Value)] -> [(Candidate, Value)]
expand1 x = filter (ok . snd) . zipp . cross (extend x, modify x) . unzip

modify      ::  Datum -> [Value] -> [Value]
modify      =   undefined

-- Making Century

type Datum     = Digit
type Candidate = Expression
type Value     = Int

candidates  =   expressions
value       =   valExpr
good        =   (100 ==)
solutions   =   filter (good . value) . candidates

extend x []  =   [[[[x]]]]
extend x es  =   concatMap (glue x) es


type Expression = [Term]
type Term       = [Factor]
type Factor     = [Digit]
type Digit      = Int

showDigit :: Digit -> String
showDigit = show

showFactor  ::  Factor -> String
showFactor  =   concatMap showDigit

showTerm  ::  Term -> String
showTerm  =   concat . intersperse "×" . map showFactor

showExpr    ::  Expression -> String
showExpr e  =   show (valExpr e) ++ " = " 
            ++  concat (intersperse "＋" (map showTerm e))

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact

valFact :: Factor -> Int
valFact =  foldl1 (\ n d -> 10 * n + d)

expressions0 :: [Digit] -> [Expression]
expressions0 = concatMap partitions . partitions

partitions ::  [a]     ->  [[[a]]]
partitions     []      =   [[]]
partitions     (x:xs)  =   [[x]:p | p <- ps] ++  [(x:ys):yss | ys:yss <- ps]
  where  ps = partitions xs

expressions  ::  [Digit] -> [Expression]
expressions  =   foldr extend []

glue :: Digit -> Expression -> [Expression]
glue x ((xs:xss):xsss) = [((x:xs):xss):xsss
                         ,([x]:xs:xss):xsss
                         ,[[x]]:(xs:xss):xsss
                         ]
--

century  ::  [Expression]
century  =   solutions [1..9]

--
type Value'    = (Value,Value,Value,Value)

value' :: Candidate -> Value'
value' ((xs:xss):xsss) = (10^length xs,valFact xs,valTerm xss,valExpr xsss)

modify' :: Datum -> [Value'] -> [Value']
modify' x [] = [(10, x, 1, 0)]
modify' x vs = concatMap (glueValue x) vs

glueValue :: Digit -> Value' -> [Value']
glueValue x (k,f,t,e) = [(10*k,k*x+f,t,e)
                        ,(10,x,f*t,e)
                        ,(10,x,1,f*t+e)
                        ]

good' :: Value -> Value' -> Bool
good' c (k,f,t,e) = f*t+e == c

ok'  :: Value -> Value' -> Bool
ok'   c (k,f,t,e) = f*t+e <= c

solutions' :: Value -> Data -> [Candidate]
solutions' c = map fst . filter (good' c . snd) . foldr (expand' c) []

expand'0 :: Datum -> Datum -> [(Candidate, Value')] -> [(Candidate, Value')]
expand'0 c x = filter (ok' c . snd) . zipp . cross (extend x, modify' x) . unzip

expand'          ::  Datum -> Datum -> [(Candidate,Value')] -> [(Candidate,Value')] 
expand' c x []   =   [([[[x]]],(10,x,1,0))]
expand' c x evs  =   concatMap (filter (ok' c . snd) . glue' x) evs

glue' :: Datum -> (Candidate, Value') -> [(Candidate, Value')]
glue' x ((xs:xss):xsss,(k,f,t,e))
  = [(((x:xs):xss):xsss,(10*k,k*x+f,t,e))
    ,(([x]:xs:xss):xsss,(10,x,f*t,e))
    ,([[x]]:(xs:xss):xsss,(10,x,1,f*t+e))
    ]

--

komachi :: Digit -> [Digit] -> [Expression]
komachi = solutions'

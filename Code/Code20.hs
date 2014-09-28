module Code20 where

countdown1    ::  Int -> [Int] -> (Expr, Value)
countdown1 n  =   nearest n . concatMap mkExprs1 . subseqs

subseqs         ::  [a] -> [[a]]
subseqs [x]     =   [[x]]
subseqs (x:xs)  =   xss ++ [x] : map  (x:) xss
                    where xss = subseqs xs

data Expr   =  Num Int | App Op Expr Expr
data Op     =  Add | Sub | Mul | Div deriving Eq
type Value  =  Int

display :: (Expr, Value) -> String
display (e,v) = showExpr e ++ " = " ++ show v

showExpr :: Expr -> String
showExpr (Num n) = show n
showExpr (App o e e')
  = "("++ showExpr e ++ showOp o ++ showExpr e' ++ ")"

showOp :: Op -> String
showOp Add = "+"
showOp Sub = "-"
showOp Mul = "*"
showOp Div = "/"

value                 ::  Expr -> Value
value (Num x)         =   x
value (App op e1 e2)  =   apply op (value e1) (value e2)

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div

legal1             ::  Op -> Value -> Value -> Bool
legal1 Add  v1 v2  =   True
legal1 Sub  v1 v2  =   (v2 < v1)
legal1 Mul  v1 v2  =   True
legal1 Div  v1 v2  =   (v1 `mod` v2 == 0)

mkExprs1      ::  [Int] -> [(Expr, Int)]
mkExprs1 [x]  =   [(Num x, x)]
mkExprs1 xs   =   [ ev |  (ys,zs) <- unmerges1 xs,
                          ev1  <- mkExprs1 ys,
                          ev2  <- mkExprs1 zs,
                          ev <- combine1 ev1 ev2 ]

unmerges1         ::  [a] -> [([a],[a])]
unmerges1 [x,y]   =   [([x],[y]),([y],[x])]
unmerges1 (x:xs)  =   [([x],xs),(xs,[x])] ++
                      concatMap (add x) (unmerges1 xs)
                      where  add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]

combine1 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine1 (e1,v1) (e2,v2)
  = [ (App op e1 e2, apply op v1 v2) | op <- ops, legal1 op v1 v2 ]

ops  ::  [Op]
ops  =   [Add,Sub,Mul,Div]

nearest :: Value -> [(Expr, Value)] -> (Expr, Value)
nearest n ((e,v) : evs)  =  if d == 0 then (e,v)
                            else search n d (e,v) evs
                            where d = abs (n-v)

search :: Value -> Value -> (Expr, Value) -> [(Expr, Value)] -> (Expr, Value)
search n d ev [] = ev
search n d ev ((e,v):evs)  |   d' ==  0  = (e,v)
                           |   d' <   d  = search n d' (e,v) evs
                           |   d' >=  d  = search n d ev evs
                               where d' = abs (n-v)

-- countdown2

countdown2    ::  Int -> [Int] -> (Expr, Value)
countdown2 n  =   nearest n . concatMap mkExprs2 . subseqs

legal2             ::  Op -> Value -> Value -> Bool
legal2 Add  v1 v2  =   (v1 <=  v2)
legal2 Sub  v1 v2  =   (v2 <   v1)
legal2 Mul  v1 v2  =   (1 < v1) /\ (v1 <= v2)
legal2 Div  v1 v2  =   (1 < v2) /\ (v1 `mod` v2 == 0)


(/\) :: Bool -> Bool -> Bool
(/\) = (&&)
infixr 3 /\

mkExprs2      ::  [Int] -> [(Expr, Int)]
mkExprs2 [x]  =   [(Num x, x)]
mkExprs2 xs   =   [ ev |  (ys,zs) <- unmerges2 xs,
                          ev1  <- mkExprs2 ys,
                          ev2  <- mkExprs2 zs,
                          ev <- combine2 ev1 ev2 ]

unmerges2         ::  [a] -> [([a],[a])]
unmerges2 [x,y]   =   [([x],[y])]
unmerges2 (x:xs)  =   [([x],xs)] ++ concatMap (add x) (unmerges2 xs)
                      where  add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]

combine2' :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine2' (e1,v1) (e2,v2)
  = [ (App op e1 e2, apply op v1 v2) | op <- ops, legal2 op v1 v2 ] ++
    [ (App op e2 e1, apply op v2 v1) | op <- ops, legal2 op v2 v1 ]

combine2 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine2 (e1,v1) (e2,v2)
  |  v1 <   v2  =  comb1 (e1,v1) (e2,v2)
  |  v1 ==  v2  =  comb2 (e1,v1) (e2,v2)
  |  v1 >   v2  =  comb1 (e2,v2) (e1,v1)
  where
    comb1 (e1,v1) (e2,v2)
        =  [(App Add e1 e2, v1 + v2),(App Sub e2 e1, v2 - v1)] ++
           if 1 < v1 then [(App Mul e1 e2, v1*v2)] ++ [(App Div e2 e1, q) | r == 0]
           else []
               where (q,r) = divMod v2 v1
    comb2 (e1,v1) (e2,v2)
        =  [(App Add e1 e2, v1 + v2)] ++
           if 1 < v1 then [(App Mul e1 e2, v1*v2),(App Div e1 e2,1)]
           else []

-- countdown3

countdown3    ::  Int -> [Int] -> (Expr, Value)
countdown3 n  =   nearest n . concatMap mkExprs3 . subseqs

mkExprs3      ::  [Int] -> [(Expr, Int)]
mkExprs3 [x]  =   [(Num x, x)]
mkExprs3 xs   =   [ ev |  (ys,zs) <- unmerges2 xs,
                          ev1  <- mkExprs3 ys,
                          ev2  <- mkExprs3 zs,
                          ev <- combine3 ev1 ev2 ]

non                      ::  Op -> Expr -> Bool
non op  (Num x)          =   True
non op1 (App op2 e1 e2)  =   op1 /= op2

legal3 :: Op -> (Expr, Value) -> (Expr, Value) -> Bool
legal3 Add (e1,v1) (e2,v2)
  =  (v1 <= v2) /\ non Sub e1 /\ non Add e2 /\ non Sub e2
legal3 Sub (e1,v1) (e2,v2)
  =  (v2 <  v1) /\ non Sub e1 /\ non Sub e2
legal3 Mul (e1,v1) (e2,v2)
  =  (1 < v1 /\ v1 <= v2) /\ non Div e1 /\ non Mul e2 /\ non Div e2
legal3 Div (e1,v1) (e2,v2)
  =  (1 < v2 /\ v1 `mod` v2 == 0) /\ non Div e1 /\ non Div e2

combine3 :: (Expr, Value) -> (Expr, Value) -> [(Expr, Value)]
combine3 (e1,v1) (e2,v2)
  |  v1 <   v2  =  comb1 (e1,v1) (e2,v2)
  |  v1 ==  v2  =  comb2 (e1,v1) (e2,v2)
  |  v1 >   v2  =  comb1 (e2,v2) (e1,v1)
  where
    comb1 (e1,v1) (e2,v2)
        =  (if non Sub e1 /\ non Sub e2 then 
            [(App Add e1 e2, v1 + v2) | non Add e2] ++ [(App Sub e2 e1, v2 - v1)]
            else []) ++
           (if 1 < v1 /\ non Div e1 /\ non Div e2 then
            [(App Mul e1 e2, v1 * v2) | non Mul e2] ++ [(App Div e2 e1, q) | r == 0]
            else [])
               where (q,r) = divMod v2 v1
    comb2 (e1,v1) (e2,v2)
        =  [(App Add e1 e2, v1 + v2) | non Sub e1, non Add e2, non Sub e2 ] ++
           (if 1 < v1 /\ non Div e1 /\ non Div e2 then
            [(App Mul e1 e2, v1 * v2) | non Mul e2 ] ++ [(App Div e1 e2,1)]
           else [])

-- countdown4

data Trie a  = Node a [(Int, Trie a)]
type Memo    = Trie [(Expr, Value)]

empty :: Memo
empty = Node [] []

fetch                       ::  Memo -> [Int] -> [(Expr,Value)]
fetch (Node es xms) []      =   es
fetch (Node es xms) (x:xs)  =   fetch (follow x xms) xs

follow                      ::  Int -> [(Int,Memo)] -> Memo
follow x xms                =   head [ m | (x',m) <- xms, x == x' ]
 
store  ::  [Int] -> [(Expr,Value)] -> Memo -> Memo
store [x] es (Node fs xms)  =  Node fs ((x,Node es []) : xms)
store (x:xs) es (Node fs xms)
  =  Node fs (yms ++ (x,store xs es m) : zms)
     where  (yms,(z,m):zms)  = break (equals x) xms
            equals x (z,m)   = (x == z)

mkExprs4 :: Memo -> [Int] -> [(Expr,Value)]
mkExprs4 memo [x] = [(Num x, x)]
mkExprs4 memo xs  = [ ev |  (ys,zs) <- unmerges2 xs,
                            ev1 <- fetch memo ys,
                            ev2 <- fetch memo zs,
                            ev <- combine3 ev1 ev2 ]

countdown4    ::  Int -> [Int] -> (Expr,Value)
countdown4 n  =   nearest n . extract . memoise . subseqs

memoise         ::  [[Int]] -> Memo
memoise         =   foldl insert empty

insert          ::  Memo -> [Int] -> Memo
insert memo xs  =   store xs (mkExprs4 memo xs) memo

extract :: Memo -> [(Expr, Value)]
extract (Node es xms)  =   es ++ concatMap (extract . snd) xms

-- countdown5

data Tree = Tip Int | Bin Tree Tree

type Memo' = Trie [Tree]

mkTrees           ::  Memo' -> [Int] -> [Tree]
mkTrees memo [x]  =   [Tip x]
mkTrees memo xs   =   [Bin t1 t2 |  (ys,zs) <- unmerges2 xs,
                                    t1 <- fetch' memo ys,
                                    t2 <- fetch' memo zs]

empty'  ::   Memo'
empty'  =   Node [] []

fetch'                       ::  Memo' -> [Int] -> [Tree]
fetch' (Node es xms) []      =   es
fetch' (Node es xms) (x:xs)  =   fetch' (follow' x xms) xs

follow'                      ::  Int -> [(Int,Memo')] -> Memo'
follow' x xms                =   head [ m | (x',m) <- xms, x == x' ]

store'  ::  [Int] -> [Tree] -> Memo' -> Memo'
store' [x] es (Node fs xms)  =  Node fs ((x,Node es []) : xms)
store' (x:xs) es (Node fs xms)
  =  Node fs (yms ++ (x,store' xs es m) : zms)
     where  (yms,(z,m):zms)  = break (equals x) xms
            equals x (z,m)   = (x == z)

toExprs :: Tree -> [(Expr, Value)]
toExprs (Tip x)      =   [ (Num x, x) ]
toExprs (Bin t1 t2)  =   [ev | ev1 <- toExprs t1, ev2 <- toExprs t2,
                               ev <- combine3 ev1 ev2]

memoise'  ::  [[Int]] -> Memo'
memoise'  =   foldl insert' empty'

insert' :: Memo' -> [Int] -> Memo'
insert' memo xs = store' xs (mkTrees memo xs) memo

countdown5 :: Value -> [Int] -> (Expr, Value)
countdown5 n = nearest n . concatMap toExprs . extract' . memoise' . subseqs

extract' :: Memo' -> [Tree]
extract' (Node es xms) =   es ++ concatMap (extract' . snd) xms


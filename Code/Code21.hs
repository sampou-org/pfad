module Code21 where

data Tree a = Leaf a | Node [Tree a] deriving (Show)

instance Functor Tree where
  fmap f (Leaf x)  = Leaf (f x)
  fmap f (Node ts) = Node (map (fmap f) ts)

fold1        ::  (Either a [b] -> b) -> Tree a -> b
fold1 f t    =   case t of
                   Leaf x   -> f (Left x)
                   Node ts  -> f (Right (map (fold1 f) ts))
unfold1      ::  (b -> Either a [b]) -> b -> Tree a
unfold1 g x  =   case g x of
                   Left y    -> Leaf y
                   Right xs  -> Node (map (unfold1 g) xs)

hylo1        ::  (Either a [b] -> b) -> (c -> Either a [c]) -> c -> b
hylo1 f g x  =   case g x of
                   Left y    -> f (Left y)
                   Right xs  -> f (Right (map (hylo1 f g) xs))

-- (21.1)

fold                ::  (a -> b) -> ([b] -> b) -> Tree a -> b
fold f _ (Leaf x)   =   f x
fold f g (Node ts)  =   g (map (fold f g) ts)

unfold          ::  (b -> Bool) -> (b -> a) -> (b -> [b]) -> b -> Tree a
unfold p v h x  =   if p x then Leaf (v x) else Node (map (unfold p v h) (h x))

{-
hylo x = if p x then f (v x) else g (map hylo (h x))
-}

data LTree a = LLeaf a | LNode a [LTree a] deriving (Show)

instance Functor LTree where
  fmap f (LLeaf x)    = LLeaf (f x)
  fmap f (LNode x xs) = LNode (f x) (map (fmap f) xs)

fill      ::  (a -> b) -> ([b] -> b) -> Tree a -> LTree b
fill f g  =   fold (lleaf f) (lnode g)

lleaf :: (a -> b) -> a -> LTree b
lleaf f x   =  LLeaf (f x)

lnode :: ([a] -> a) -> [LTree a] -> LTree a
lnode g ts  =  LNode (g (map label ts)) ts

label :: LTree a -> a
label (LLeaf x)     = x
label (LNode x _)  = x

hylo        ::  ([a] -> b) -> ([b] -> b) -> ([a] -> [[a]]) -> [a] -> b
hylo f g h  =   fold f g . mkTree h

mkTree :: ([a] -> [[a]]) -> [a] -> Tree [a]
mkTree h = unfold single id h

single      ::  [a] -> Bool
single [_]  =   True
single _    =   False

-- Three examples

split     ::  [a] -> [[a]]
split xs  =   [take n xs, drop n xs]  where n = length xs `div` 2

isegs     ::  [a] -> [[a]]
isegs xs  =   [init xs, tail xs]

recover :: [[a]] -> [a]
recover xss = head (head xss) : last xss

minors         ::  [a] -> [[a]]
minors [x,y]   =   [[x],[y]]
minors (x:xs)  =   map (x:) (minors xs) ++ [xs]

-- Building a nexus

wrap :: a -> [a] 
wrap = (:[])

-- (1)  h = split
type Layer1 = []

mkNexus1 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus1 f g = label . extractL1 . until singleL1 (stepL1 g) . initialL1 f

initialL1    ::  ([a] -> b) -> [a] -> Layer1 (LTree b)
stepL1       ::  ([b] -> b) -> Layer1 (LTree b) -> Layer1 (LTree b)
singleL1     ::  Layer1 (LTree b) -> Bool
extractL1    ::  Layer1 (LTree b) -> LTree b

initialL1 f  =   map (lleaf f . wrap)
stepL1 g     =   map (lnode g) . group1
singleL1     =   single
extractL1    =   head

group1           ::  [a] -> [[a]]
group1 []        =   []
group1 (x:y:xs)  =   [x,y] : group1 xs

-- (2)  h = split
type Layer2 = Layer1

mkNexus2 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus2 f g = label . extractL2 . until singleL2 (stepL2 g) . initialL2 f

initialL2    ::  ([a] -> b) -> [a] -> Layer2 (LTree b)
stepL2       ::  ([b] -> b) -> Layer2 (LTree b) -> Layer2 (LTree b)
singleL2     ::  Layer2 (LTree b) -> Bool
extractL2    ::  Layer2 (LTree b) -> LTree b

initialL2    =   initialL1
stepL2 g     =   map (lnode g) . group2
singleL2     =   singleL1
extractL2    =   extractL1

group2           ::  [a] -> [[a]]
group2 [x]       =   []
group2 (x:y:xs)  =   [x,y] : group2 (y:xs)

-- (3) h = minors

type Layer3 a = [Tree a]

mkNexus3 :: ([a] -> b) -> ([b] -> b) -> [a] -> b
mkNexus3 f g = label . extractL3 . until singleL3 (stepL3 g) . initialL3 f

initialL3  ::  ([a] -> b) -> [a] -> Layer3 (LTree b)
stepL3     ::  ([b] -> b) -> Layer3 (LTree b) -> Layer3 (LTree b)
singleL3   ::  Layer3 (LTree b) -> Bool
extractL3  ::  Layer3 (LTree b) -> LTree b

initialL3 f = map (Leaf . lleaf f . wrap)
singleL3    = single
extractL3   = extract . head
              where  extract (Leaf x)    = x
                     extract (Node [t])  = extract t

stepL3 g = map (mapTree (lnode g)) . group3

group3 :: [Tree a] -> [Tree [a]]
group3 [_] = []
group3 (Leaf x : vs)
  = Node [Leaf [x,y] | Leaf y <- vs] : group3 vs
group3 (Node us : vs)
  = Node (zipWith combine (group3 us) vs) : group3 vs

combine :: Tree [a] -> Tree a -> Tree [a]
combine (Leaf xs) (Leaf x)   =  Leaf (xs ++ [x])
combine (Node us) (Node vs)  =  Node (zipWith combine us vs)

mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree = fmap

-- Why build the nexus?

-- case:  h = isegs

solve2      ::  ([a] -> b) -> ([b] -> b) -> [a] -> b
solve2 f g  =   head . until single (map g . group2) . map (f . wrap)

-- case:  h = minors

solve3      ::  ([a] -> LTree b) -> ([LTree b] -> LTree b) -> [a] -> LTree b
solve3 f g  =   extractL3 . until singleL3 (step g) . map (Leaf . f . wrap)
step        ::  ([a] -> b) -> [Tree a] -> [Tree b]
step g      =   map (mapTree g) . group3

-- optimal bracketing

uncats         ::  [a] -> [([a], [a])]
uncats [x,y]   =   [([x],[y])]
uncats (x:xs)  =   ([x],xs) : map (cons x) (uncats xs)
                   where  cons x (ys,zs) = (x:ys, zs)

-- for Countdown

lnode2 :: ([(a, a)] -> a) -> [LTree a] -> LTree a
lnode2 g [u,v]  =   LNode (g (zip (lspine u) (rspine v))) [u,v]

lspine, rspine          ::  LTree a -> [a]
-- lspine (LLeaf x)        =   [x]
-- lspine (LNode x [u,v])  =   lspine u ++ [x]
lspine = lspine' []
  where
    lspine' acc (LLeaf x)       = x : acc
    lspine' acc (LNode x [u,v]) = lspine' (x:acc) u

rspine (LLeaf x)        =   [x]
rspine (LNode x [u,v])  =   [x] ++ rspine v

unmerges :: [a] -> [([a], [a])]
unmerges [x,y]   =   [([x],[y])]
unmerges (x:xs)  =   [([x],xs)] ++ concatMap (add x) (unmerges xs)
                     where  add x (ys,zs) = [(x:ys,zs),(ys,x:zs)]

traverse :: [LTree a] -> [a]
traverse [] = []
traverse ts = map label ts ++ traverse (concatMap subtrees ts)

subtrees :: LTree t -> [LTree t]
subtrees (LLeaf _)     =   []
subtrees (LNode _ ts)  =   ts

forest :: Int -> [LTree a] -> [LTree a]
forest _ (LLeaf x : ts) = LLeaf x : ts
forest k (LNode x us : vs)
  = LNode x (forest k (drop k us)) : forest (k+1) vs

lnode3 :: ([(a, a)] -> a) -> [LTree a] -> LTree a
lnode3 g ts = LNode (g (zip xs (reverse ys))) ts
              where (xs,ys) = halve (traverse (forest 0 ts))

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

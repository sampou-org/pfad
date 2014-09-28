module Code09 where

type Person = Int

infix 1 =>>

-- Theo

lhd :: [Person] -> [Person] -> Bool
cs `lhd` ps  =   and [ x `knows` y && (y `knows` x =>> x `elem` cs) | x <- ps, y <- cs ]

(=>>)    ::  Bool -> Bool -> Bool
x =>> y  =   not x || y

knows :: Person -> Person -> Bool
knows = undefined

cclique0 ps = head (filter (`lhd` ps) (subseqs ps))

subseqs         ::  [a] -> [[a]]
subseqs []      =   [[]]
subseqs (x:xs)  =   map (x:) (subseqs xs) ++ subseqs xs

nonmember       ::  Person -> [Person] -> Bool
nonmember p cs  =   and [ p `knows` c && not (c `knows` p) | c <- cs ]

member p ps cs  =   and [x `knows` p && (p `knows` x =>> x `elem` cs) | x <- ps ]


cclique0' = head . ccliques

ccliques []      =   [[]]
ccliques (p:ps)  =   map (p:) (filter (member p ps) css)
                 ++  filter (nonmember p) css
                     where  css = ccliques ps

-- Mary

clique' = foldr op []
op p cs | null cs  = [p]
        | not (p `knows` c) = [p]
        | not (c `knows` p) = cs
        | otherwise         = p:cs
          where c = head cs

-- start point for derivation

cclique = f . subseqs'
  where
    f (ps,css) = head (filter (`lhd` ps) css)

subseqs' = foldr step ([],[[]])
step x (xs,xss) = (x:xs, map (x:) xss ++ xss)


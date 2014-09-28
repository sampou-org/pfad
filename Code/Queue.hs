module Queue (Queue,insert,remove,empty,elems,isEmpty) where

type Queue a = ([a],[a],[a])

empty :: Queue a
empty =  ([],[],[])
isEmpty :: Queue a -> Bool
isEmpty (xs,_,_) = null xs
insert :: Queue a -> a -> Queue a
insert (xs,ys,zs) x = mkValid (xs,x:ys,zs)
remove :: Queue a -> (a,Queue a)
remove (x:xs,ys,zs) = (x,mkValid (xs,ys,zs))
elems :: Queue a -> [a]
elems (xs,ys,_) = xs ++ reverse ys

mkValid (xs,ys,[]) = (zs,[],zs)
                     where  zs = rot xs ys []
mkValid (xs,ys,z:zs) = (xs,ys,zs)

revcat [] ys = ys
revcat (x:xs) ys = revcat xs (x:ys)

rot :: [a] -> [a] -> [a] -> [a]
rot xs ys zs = xs ++ revcat ys zs

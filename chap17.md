--
title: 17. Knuth-Morris-Prattアルゴリズム
date: 2014-11-07
--

### データの精緻化

#### 原文には明確に書いてないが (17.2) から rep . op x = op' (rep x) がいえる

```
   op' r = rep . op (abs r)
⇒ {- r に rep x を代入 -}
   op' (rep x) = rep . op (abs (rep x))
⇒ {- abs (rep x) = x だから -}
   op' (rep x) = rep . op x
```

#### op' の定義の otherwise = op' (rep (split ws (tail us))) x のところ

```
rep(op の定義の otherwise のときの値)
=
rep (op (split ws (tail us)) x)
= {- 上記「rep . op x = op' (rep x) がいえる」の x を (split ws (tail us)) に y を x にあてはめれば -}
op' (rep (split ws (tail us))) x
```


#### grep は以下のように定義できる

```
grep l (us,[])
=
Node (us, []) l (right us [])
=
Node (us,[]) l Null

grep l (us,v:vs)
=
Node (us,v:vs) l (right us (v:vs))
=
Node(us,v:vs) l (grep (left (us + [v]) vs) (us+[v],vs))
=
Node(us,v:vs) l (grep (if null us then root else op′ (left us vs) v) (us+[v],vs))
= {- if null us then root else op′ (left us vs) v = op′ l v を示せばよい。後で示す -}
Node(us,v:vs) l (grep (op′ l v) (us+[v],vs))

if null us then root else op′ (left us vs) v = op′ l v の証明

rep (us, vs) = grep (left us vs) (us, vs) なので
grep l (us,v:vs) として呼ばれるときは l = left us (v:vs) になっている。

us = [] のとき
if null us then root else op′ (left us vs) v = root

一方
op′ l v = op' (left [] (v:vs)) v = op' Null v = root
で一致。

us = u:us' のとき
if null us then root else op′ (left us vs) v
=
op' (left (u:us') vs) v

一方
op′ l v
=
op' (left (u:us') (v:vs)) v
= {- left (u:us') (v:vs) = left (u:us') vs を示せばよい。後から示す -}
op' (left (u:us') vs) v
で一致。

left (u:us') (v:vs) = left (u:us') vs の証明

left (u:us') (v:vs)
= {- left の定義。left の第2引数は影響しないことに注意 -}
rep (split ws us')
= {- left の定義。left の第2引数は影響しないことに注意 -}
left (u:us') vs
```

---
title: 02. 上位者問題
date: 2014-11-14
---

### 分割統治法

#### tableの定義より d ≤ tcount x tys′

準備として二つの事実を示しておく。

まず一つめ、
```
tcount x (table ys) = length (filter (x<) ys)
```

なぜなら
```
tcount x (table ys)
= {- (2.1) -}
length (dropWhile ((x>=) . fst) (table ys))
= {- table -}
length (dropWhile ((x>=) . fst) (sort [(z, scount z zs) | z:zs <- tails ys]))
= {- sort したものを (x>=) で dropWhile すると残るのは (x<) なものだけなので -}
length (filter ((x<) . fst) (sort [(z, scount z zs) | z:zs <- tails ys]))
= {- length . filter p . sort = length . filter p だから -}
length (filter ((x<) . fst) [(z, scount z zs) | z:zs <- tais ys])
= {- length . filter (p . f) = length . filter p . map f だから -}
length (filter ((x<)) (map fst [(z, scount z zs) | z:zs <- tails ys]))
=
length (filter ((x<)) [z | z:zs <- tails ys])
= {- [z | z:zs <- tails ys] = ys だから -}
length (filter ((x<)) ys)
```

二つめ、
```
table ys = (y,d): tys' とする。
ys から y を一つ削除したものを ys' とすると tys' = table ys'。
（ys に要素が y と等しいものが複数あるときはその中でも一番右にあるものを削除。）
```
なぜなら
```
[(z, scount z zs) | z:zs <- tails ys] と [(z, scount z zs) | z:zs <- tails ys'] を比べると
y は ys の中で最小なので y が削除されていても scount z zs に影響を与えない。
結局違いは (y,d) があるかないかだけの違いになる。したがって
sort [(z, scount z zs) | z:zs <- tails ys] = (y,d): sort [(z, scount z zs) | z:zs <- tails ys']
つまり
table ys = (y,d): table ys'
```

準備ができたので本題に入る。

tys@((y,d): tys' = table ys とする。
上で述べた準備の二つめから tys' = table ys' と書ける。ここで ys' は ys から y を一つ削除したもの。

```
d
= {- table の作られ方より -}
scount y zs （ここで y: zs ∈ tails ys）
= {- scount の定義 -}
length (filter (y<) zs)
<= {- zs は ys の部分列だから -}
length (filter (y<) ys)
= {- ys と ys' の違いは一つの y だけなので -}
length (filter (y<) ys')
= {- 上で述べた準備の一つめより -}
tcount y (table ys')
= {- tys' = table ys' -}
tcount y tys'
= {- ここでは x = y のケースで考えているから -}
tcount x tys'
```

#### (2.1) よりtcount x tys = tcount x tys′ である

```
tcount x tys
= {- (2.1) -}
length (dropWhile ((x >=) . fst) tys)
= {- tys = (y,d): tys' -}
length (dropWhile ((x >=) . fst) ((y,d): tys'))
= {- ここでは x = y のケースで考えているので x >= fst (y,d) -}
length (dropWhile ((x >=) . fst) tys')
= {- (2.1) -}
tcount x tys′
```


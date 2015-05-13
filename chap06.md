---
title: 06. 小町算
date: 2015-05-13
---

#### 小さな理論

#### 2つの運算を合わせて、以下に到達する

```
map (fork (id,value)) . extend' x
= { 2つの運算を合わせて }
filter (ok . snd) . zip . cross (extend x, modify x) . fork (map id, map value)
= { id 挿入 }
filter (ok . snd) . zip . cross (extend x, modify x) . id . fork (map id, map value)
= { id = unzip . zip }
filter (ok . snd) . zip . cross (extend x, modify x) . unzip . zip . fork (map id, map value)
= { expand x = filter (ok . snd) . zip . cross (extend x, modify x) . unzip とおく }
expand x . zip . fork (map id, map value)
= { (6.9) }
expand x . map (fork (id,value))
```

これで31ページ上の「以下を満たす expand を求めなければならない」の
```
map (fork (id,value)) . extend' x = expand x . map (fork (id,value))
```
が成り立っていることが確認できた。
このときそのすぐ下に書いてあるとおり
```
candidates = foldr expand []
```
である。

一方、29ページ中の
```
solutions = filter (goot . value) . candidates
```
は31ページの「新しい版の candidates は、候補とそれらの値との対からなるリストを返す」の変更に伴って
```
solutions = map fst . filter (goot . snd) . candidates
```
これに上記の candidates = foldr expand [] を使うと
```
solutions = map fst . filter (goot . snd) . foldr expand []
```

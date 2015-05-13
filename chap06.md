---
title: 06. 小町算
date: 2015-05-13
---

### 小さな理論

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

### 小町算

#### modify x (k,f,t,e) = [(10*k,k*x+f,t,e), (10,x,f*t,e),(10,x,1,f*t+e)]

```
k: 数字を数式の一番左に来ている数の先頭桁として付け加えるときの桁を表す数(10の累乗）
f: 数式の一番左に来ている数
t: 数式の一番左に来ている数の右に掛け算されている数（掛け算されていないときは 1）
e: 数式の一番左に来ている掛け算の項の右に足し算されている数（足し算されていないときは 0）
```

例えば数式 45*6+7*8+9 に対応する (k,f,t,e) は (100,45,6,7*8+9)。

この数式の左に 3 をおいた数式には次のような三通りがある。
```
345*6+7*8+9
3*45*6+7*8+9
3+45*6+7*8+9
```
それぞれの数式に対応する (k,f,t,e) は以下のようになる。
```
345*6+7*8+9 => (1000,345,6,7*8+9)
3*45*6+7*8+9 => (10,3,45*6,7*8+9)
3+45*6+7*8+9 => (10,3,1,45*6+7*8+9)
```
これがそれぞれ
```
(10*k, k*x + f, t, e)
(10, x, f*t, e)
(10, x, 1, f*t + e)
```
に対応している。

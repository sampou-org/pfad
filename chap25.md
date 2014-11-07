--
title: 25. 整数算術符号
date: 2014-11-07
--

### 新しい定義

#### 関数 ebit は，e2 をまたぐ区間に対しては Nothing を返すので,encode3 は幅が e1 以上の区間のみを▶で狭める

```
ebit は,e2 をまたぐ区間に対しては Nothing を返す
->
stream は ebit が Nothing を返したら enarrow を呼ぶ
->
enarrow に e2 をまたぐ区間が渡される
->
enarrow は e2 をまたぐ区間を完全拡張してから▶で狭める
->
前節で e2 をまたぐ区間に対して完全拡張すると幅が e1 以上の区間になると述べている
->
encode3 は幅が e1 以上の区間のみを▶で狭める
```

### ヘルパー関数

#### floor(widen n (2^e * toFrac (encode3' m (n,i) (x:xs)))) ∈ i ▶︎ interval m x

```
floor(widen n (2^e * toFrac (encode3' m (n,i) (x:xs))))
= {- encode3' m (n,i) (x:xs) = encode3' (adapt m x) (n,i ▶︎ interval m x) xs -}
floor(widen n (2^e * toFrac (encode3' (adapt m x) (n,i ▶︎ interval m x) xs)))
∈ {- floor(widen n (2^e * toFrac (encode3' m (n,i) xs)))) ∈ i で m を adapt m x に、i を i ▶︎ interval m x に読み替えて -}
i ▶︎ interval m x
```

### 漸進的な復号

#### ビット列 bs は，全長が e 以上になるように 1:repeat 0 を追加して拡張しなければならない

この 1 は24章の「すなわち toFrac bs は，bs の末尾に 1ビットを追加してから，それを通常の方法で分数に変換する」の 1ビットに対応している。

### 付録

#### 訳注： n = 0 の場合，stream の定義より f s = Nothing であるから上述の場合に帰着する

後で書く。

#### 訳注： encode3' m ei xs の長さが 0 の場合は

後で書く。

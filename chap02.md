---
title: 02. 上位者問題
date: 2014-11-13
---

### 分割統治法

#### tableの定義より d ≤ tcount x tys′

後で書く。

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


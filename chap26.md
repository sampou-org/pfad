--
title: 26. Schorr-Waiteアルゴリズム
date: 2014-11-07
--

### 連結リストによるスタックの表現

#### setr (swing g y x) y (right g y) = setl g y x だから

```
   setr (swing g y x) y (right g y)
 =   {- definition of swing -}
   setr (setr (setl g y x) y (left g y)) y (right g y)
 =   {- since the result of inner "setr" is overwritten by outer "setr" -}
   setl g y x
```
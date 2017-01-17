---
title: 19. 簡単な数独ソルバー
date: 2017-01-17
---

## cols の対合性

```haskell
cols :: [[a]] -> [[a]]
cols [xs]     = map (:[]) xs
cols (xs:xss) = zipWith (:) xs (cols xss)
```

``cols (cols xss) = xss``
``xss``上の帰納法

``[[x]]``の場合

```
  cols (cols [[x]])
=   { cols の定義 }
  cols (map (:[]) [x])
=   { map の定義 }
  cols ([x] : map (:[]) [])
=   { map の定義 }
  cols ([x] : [])
=   { リストの記法 }
  cols [[x]]
=   { 同様の運算を繰り返す }
  [[x]]
```

``[x:xs]``の場合

```
  cols (cols [x:xs])
=   { colsの定義 }
  cols (map (:[]) (x:xs))
=   { map の定義 }
  cols ([x] : map (:[]) xs))
=   { cols の定義 }
  cols ([x] : cols [xs])
=   { cols の定義 }
  zipWith (:) [x] (cols (cols [xs]))
=   { 帰納法の仮定 }
  zipWith (:) [x] [xs]
=   { zipWith の定義 }
  [x:xs]
```

``xs:xss``の場合

```
  cols (cols (xs:xss))
=   { cols の定義 }
  cols (zipWith (:) xs (cols xss))
=   { zipWith (:) xs = zipWith (++) (map (:[]) xs) であるから }
  cols (zipWith (++) (map (:[]) xs) (cols xss))
=   { cols の定義 }
  cols (zipWith (++) (cols [xs]) (cols xss))
=   { zipWith (++) = (⊕) とする }
  cols (cols [xs] ⊕ cols xss)
=   { 補題: cols (xss ⊕ yss) = cols xss ++ cols yss }
  cols (cols [xs]) ++ cols (cols xss)
=   { 帰納法の仮定 }
  [xs] ++ xss
=   { (++) の定義 }
  xs:xss
```

## 補題

```
cols (xss ⊕ yss) = cols xss ++ cols yss
  where
    (⊕) = zipWith (++)
```

この補題には以下の付帯条件がある．

```
  (1) 0 < length xss = length yss
  (2) 0 < lenght (head xss)
  (3) 0 < length (head yss)
  (4) all (length (head xss) ==) xss
  (5) all (length (head yss) ==) xss
```

証明は``xss``上の帰納法による

``[xs]``の場合
付帯条件(1)より``yss = [ys]``

```
  cols ([xs] ⊕ [ys])
=   { ⊕ の定義 }
  cols (zipWith (++) [xs] [ys])
=   { zipWith の定義 }
  cols ((xs++ys) : zipWith (++) [] [])
=   { zipWith の定義 }
  cols ((xs++ys):[])
=   { リストの記法 }
  cols [xs++ys]
=   { cols 定義 }
  map (:[]) (xs++ys)
=   { map-++ 則 }
  map (:[]) xs ++ map (:[]) ys
=   { cols の定義 }
  cols xs ++ cols ys
```

``(xs:xss)``の場合
付帯条件(1)より``yss``は``ys : yss``と表せる

```
  cols ((xs:xss) ⊕ (ys:yss))
=   { ⊕ の定義 }
  cols (zipWith (++) (xs:xss) (ys:yss))
=   { zipWith の定義 }
  cols ((xs++ys) : zipWith (++) xss yss)
=   { ⊕ の定義 }
  cols ((xs++ys) : (xss ⊕ yss))
=   { cols の定義 }
  zipWith (:) (xs++ys) (cols (xss ⊕ yss))
=   { 帰納法の仮定より }
  zipWith (:) (xs++ys) (cols xss ++ cols yss)
=   { 付帯条件 (4) より length xs = length (cols xss) なので }
  zipWith (:) xs (cols xss) ++ zipWith (:) ys (cols yys)
=   { cols の定義より }
  cols (xs:xss) ++ cols (ys:yss)
```

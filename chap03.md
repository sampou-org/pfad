--
title: 03. 鞍型探索の改良
date: 2014-11-07
--

#### log A(m,n) = Ω(m * log(1+n/m) + n * log(1+m/n))

スターリングの公式
```
n! 〜 √(2πn) * n^n/e^n
```
を使う。

```
A(m,n) = (m+n)!/(m! * n!) 
〜 √(2π(m+n)) * (m+n)^(m+n)/e^(m+n) / (√(2πm) m^m/e^m * √(2πn) n^n/e^n)
= √((m+n)/(2πmn)) * (m+n)^(m+n) / (m^m * n^n)
= √((1/m + 1/n)/(2π)) * (m+n)^m / m^m * (m+n)^n / n^n
= √((1/m + 1/n)/(2π)) * (1+n/m)^m * (1+m/n)^n
log を取ると
log A(m,n) 〜 1/2 * log((1/m + 1/n)/(2π)) + m * log(1+n/m) + n * log(1+m/n)
= Ω(m * log(1+n/m) + n * log(1+m/n))
```


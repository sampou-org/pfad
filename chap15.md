--
title: 15. すべての共通接頭辞
date: 2014-11-07
--

### 鍵となる性質

#### 図 15.1 step xs (as,i,p,k) の引数

as： [llcp xs (drop i xs)| i <- [0..k-1]]  
i： k-1 までの step で i+p がよりよい選択肢になったときの i  
p： k-1 までの step で i+p がよりよい選択肢になったときの p  
k: これから llcp xs (drop k xs) を計算する k  


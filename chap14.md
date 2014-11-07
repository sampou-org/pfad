--
title: 14. 最後の接尾辞
date: 2014-11-07
--

### borders

#### ［ys| ys <- tails xs, ys \sqsubseteq xs] の２番目が border (xs) に、３番目が border (border xs) になる理由

上記の要素の条件は xs の接尾辞かつ接頭辞であるということである。  
２番目を as とする。上記リストの中で xs の次に長いので as = border xs。  
３番目を bs とする。bs は xs の接尾辞で as より長さが短いので as の接尾辞でもある。  
同様に as の接頭辞でもある。  
cs = border as とすると cs も as の接尾辞かつ接頭辞であり as の次に長いものなので  
cs は bs に等しいか、等しくなければ bs より長さが長い。  
cs は as の接尾辞かつ接頭辞なので xs の接尾辞かつ接頭辞でもある。  
もし cs が bs に等しくなければ cs は上記リストの中で as より短く bs より長い要素ということになる。  
これは as が２番目で bs が３番目ということに反するので cs = bs  
よって bs = cs = border as = border (border xs)  



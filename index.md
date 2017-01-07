---
title: 関数プログラミング 珠玉のアルゴリズムデザイン
date: 2017-01-07
---

## このサイトについて

このサイトは2014年11月にオーム社より発刊されました[『関数プログラミング 珠玉のアルゴリズムデザイン』](http://www.amazon.co.jp/gp/product/4274050645/ref=as_li_ss_tl?ie=UTF8&camp=247&creative=7399&creativeASIN=4274050645&linkCode=as2&tag=philoprogramm-22)を楽しむためのサポートサイトです．

<a href="http://www.amazon.co.jp/gp/product/4274050645/ref=as_li_ss_tl?ie=UTF8&camp=247&creative=7399&creativeASIN=4274050645&linkCode=as2&tag=philoprogramm-22"><img alt="関数プログラミング 珠玉のアルゴリズムデザイン" src="http://ecx.images-amazon.com/images/I/61uv43I3omL.jpg" height="250px"></a>
<a href="http://www.amazon.co.jp/gp/product/0521513383/ref=as_li_ss_tl?ie=UTF8&camp=247&creative=7399&creativeASIN=0521513383&linkCode=as2&tag=philoprogramm-22"><img alt="Pearls of Functional Algorithm Design" src="http://ecx.images-amazon.com/images/I/51JRG-YGgOL.jpg" height="250px"></a>

原書カバーの Puzzle no. 234 は ALGORITHM の 9 文字を使った Sudoku パズルになっています．

## 演算子記号とASCII文字列との対応

| 本書の記号 | HaskellのASCII文字 | 本書の記号 | HaskellのASCII文字 |
|:----------:|:------------------:|:----------:|:------------------:|
| $\le$      | `<=`               | $\ge$      | `>=`               |
| $\vee$     | `||`               | $\wedge$   | `&&`               |
| $\cdot$    | `.`                | $\neq$     | `/=`               |
| $\in$      | `` `elem` ``       | $\notin$   | `` `notElem` ``    |
| $\sqsubseteq$ | `` `isPrefixOf` `` | $\lambda$ | `` \ ``          |
| div        | `` `div` ``        | mod        | `` `mod` ``        |
| min        | `` `min` ``        | max        | `` `max` ``        |
| knows      | `` `knows` ``      |            |                    |

演算子以外の構文記号の一部については，GHCの言語拡張``UnicodeSyntax``を有効にするとソースコード中に記述可能です．
[GHCユーザーガイド](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unicode-syntax)

## 関連記事

読書メモなどを書いていただいた記事など，見つけたもの，教えてもらったもの．

- [【随時追記予定】読書メモ：関数プログラミング 珠玉のアルゴリズムデザイン](http://xenophobia.hatenablog.com/entry/2014/11/15/031820)
- [「関数プログラミング 珠玉のアルゴリズムデザイン」をScalaで実装してみる まとめ](http://qiita.com/qtamaki@github/items/176b4332da8e1e481fad)
- [珠玉のアルゴリズムデザイン１９章の数独ソルバー読みました](http://nihemak.hatenablog.com/entry/2015/01/11/210124)
- [1. The smallest free number (最小自然数) 関数プログラミング 珠玉のアルゴリズムデザイン](http://phasetr.com/blog/2017/01/05/smallest-free-number-pearls-of-func-algorithm/)

## 質問・感想・誤りの指摘などについて

この翻訳書の内容に関するご質問，ご感想，誤りのご指摘などございましたら，

- [github](https://github.com/sampou-org/pfad/issues)にイシューを開いていただくか，
- twitter で @nobsun 宛にハッシュタグ #pfadj をつけて呟いていただくか，
- [chaton haskell-ja](http://chaton.practical-scheme.net/haskell-ja/)でコメントを投稿していただくか，
- haskell-jp at googlegroups dot comに参加していただいてメッセージを投稿いただく．

などしていただければ幸いです．
ご指摘などの場合は件名などにページ番号を入れていただけると嬉しいです．
十分な回答，即座の返信はできないことがあることを予めご了承下さい．


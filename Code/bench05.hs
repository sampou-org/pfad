module Main where

import Data.Array
import Data.List
import Data.Ord

import Code05

main :: IO ()
main =  do 
  { print $ length $ ex10
  ; print $ length $ ex20
  ; print $ length $ ex50
  ; print $ length $ ex100
  ; print $ length $ ex200
  ; print $ length $ ex500
  ; print $ length $ ex1000
  ; print $ length $ ex2000
  }
  where
    ex10 = sortsums x10 x10
    ex20 = sortsums x20 x20
    ex50 = sortsums x50 x50
    ex100 = sortsums x100 x100
    ex200 = sortsums x200 x200
    ex500 = sortsums x500 x500
    ex1000 = sortsums x1000 x1000
    ex2000 = sortsums x2000 x2000

x10,x20,x50,x100,x200,x500,x1000,x2000 :: [A]
x10 = [1..10]
x20 = [1..20]
x50 = [1..50]
x100 = [1..100]
x200 = [1..200]
x500 = [1..500]
x1000 = [1..1000]
x2000 = [1..2000]

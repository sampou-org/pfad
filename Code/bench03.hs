module Main where

import Code03

main :: IO ()
main =  do 
  { print $ anne
  ; print $ theo
  ; print $ mary
  }
  where
    anne = map (flip invertA 5000) fs
    theo = map (flip invertT 5000) fs
    mary = map (flip invertM 5000) fs
    fs   = [f0,f1,f2,f3,f4]
 
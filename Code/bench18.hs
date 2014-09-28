module Main where

import Code18

main :: IO ()
main = do 
  { putStr "bfsolve puzzle1 : " >> print (bfsolve puzzle1)
  ; putStr "bfsolve puzzle2 : " >> print (bfsolve puzzle2)
  ; putStr "bfsolve puzzle3 : " >> print (bfsolve puzzle3)
  ; putStr "bfsolve puzzle4 : " >> print (bfsolve puzzle4)
  ; putStr "psolve puzzle1 : " >> print (psolve puzzle1)
  ; putStr "psolve puzzle2 : " >> print (psolve puzzle2)
  ; putStr "psolve puzzle3 : " >> print (psolve puzzle3)
  ; putStr "psolve puzzle4 : " >> print (psolve puzzle4)
  }

module Main where

import Code20

main :: IO ()
main = putStrLn $ display (countdown3 831 [1,3,7,10,25,50])

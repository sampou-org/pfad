{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Shelly
import qualified Data.Text as T
import Control.Monad

default (T.Text)

git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

git' :: T.Text -> [T.Text] -> Sh T.Text
git' = command1 "git" []

exmsg :: T.Text
exmsg = "Already up-to-date"

len :: Int
len = T.length exmsg

main :: IO ()
main = shelly $ do
  git_ "checkout" ["master"]
  msg <- git' "pull" []
  if T.take len msg == exmsg then return () else deploy

deploy :: Sh ()
deploy = do
  run_ "cabal" ["run", "pfad", "rebuild"]
  git_ "checkout" ["gh-pages"]

  files <- lsT "_site"
  forM_ files $ \file -> do
    run_ "cp" ["-r", file, "."]

  files <- findWhen test_f "_site"
  forM_ files $ \file -> do
    t <- toTextWarn file
    git_ "add" [T.drop 6 t]

  files <- findWhen test_f "css"
  forM_ files $ \file -> do
    t <- toTextWarn file
    git_ "add" [t]

  files <- findWhen test_f "js"
  forM_ files $ \file -> do
    t <- toTextWarn file
    git_ "add" [t]

  git_ "commit" ["-m", "update"]
  git_ "push" []
  
  `finally_sh` git_ "checkout" ["master"]

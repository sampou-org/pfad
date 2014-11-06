{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Shelly
import qualified Data.Text as T
import Control.Monad

default (T.Text)

git_ :: T.Text -> [T.Text] -> Sh ()
git_ = command1_ "git" []

main :: IO ()
main = shelly $ do
  git_ "checkout" ["master"]

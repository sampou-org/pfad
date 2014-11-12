{-# LANGUAGE OverloadedStrings #-}

module Main where

import Hakyll

main :: IO ()
main =  hakyllWith config $ do
  match "css/*.css" $ do
    route idRoute
    compile compressCssCompiler
  match "js/*.js" $ do
    route idRoute
    compile copyFileCompiler
  match (fromList $ ["index.md","errata.md"] ++ chaps) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls
  match "templates/*" $ compile templateCompiler

config :: Configuration
config = defaultConfiguration
       { deployCommand = "cabal run deploy"
       }

chaps :: [Identifier]
chaps = ["chap01.md"
        ,"chap02.md"
        ,"chap03.md"
        ,"chap04.md"
        ,"chap05.md"
        ,"chap06.md"
        ,"chap07.md"
        ,"chap08.md"
        ,"chap09.md"
        ,"chap10.md"
        ,"chap11.md"
        ,"chap12.md"
        ,"chap13.md"
        ,"chap14.md"
        ,"chap15.md"
        ,"chap16.md"
        ,"chap17.md"
        ,"chap18.md"
        ,"chap19.md"
        ,"chap20.md"
        ,"chap21.md"
        ,"chap22.md"
        ,"chap23.md"
        ,"chap24.md"
        ,"chap25.md"
        ,"chap26.md"
        ,"chap27.md"
        ,"chap28.md"
        ,"chap29.md"
        ,"chap30.md"
        ]

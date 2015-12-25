module Main where

import Test.DocTest
import System.Process

main :: IO ()
main = do
  files <- lines <$> readProcess "find" ["src", "-type", "f", "-name", "*.hs"] []
  doctest $ [] ++ files

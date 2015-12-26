module NLP100.Chapter01.Internal where

import Data.List.Split
import Text.Regex

onlyWord :: String -> String
onlyWord n = subRegex (mkRegex "[,\\.]") n ""

splitWords :: String -> [String]
splitWords = splitOn " " . onlyWord

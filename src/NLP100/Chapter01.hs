module NLP100.Chapter01 where

import Data.List.Split
import Text.Regex

-- | 00. 文字列の逆順
--
-- 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．
--
knock00 :: String -> String
knock00 [] = ""
knock00 (x:xs) = knock00 xs ++ [x]

-- | 01. 「パタトクカシーー」
--
-- 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．
--
knock01 :: String -> String
knock01 n = [n!!0] ++ [n!!2] ++ [n!!4] ++ [n!!6]

-- | 02. 「パトカー」＋「タクシー」＝「パタトクカシーー」
--
-- パトカー」＋「タクシー」の文字を先頭から交互に連結して
-- 文字列「パタトクカシーー」を得よ．
--
knock02 :: String -> String -> String
knock02 [] [] = ""
knock02 (x:xs) [] = [x] ++ knock02 xs []
knock02 [] (y:ys) = [y] ++ knock02 [] ys
knock02 (x:xs) (y:ys) = [x] ++ [y] ++ knock02 xs ys

-- | 03. 円周率
--
-- Now I need a drink, alcoholic of course,
-- after the heavy lectures involving quantum mechanics.
--
-- という文を単語に分解し，各単語の（アルファベットの）文字数を
-- 先頭から出現順に並べたリストを作成せよ．
knock03 :: String -> String
knock03 n = (show $ head lengthes) ++ "." ++ (concat $ map show $ tail lengthes)
  where onlyword = subRegex (mkRegex "[,\\.]") n ""
        lengthes = map length $ splitOn " " onlyword


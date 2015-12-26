module NLP100.Chapter01 where

import NLP100.Chapter01.Internal
import Data.List

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
knock03 n = headWordLengthStr n ++ "." ++ tailWordLengthStr n
  where
    wordLengthes :: String -> [Int]
    wordLengthes = map length . splitWords
    headWordLengthStr :: String -> String
    headWordLengthStr = show . head . wordLengthes
    tailWordLengthStr :: String -> String
    tailWordLengthStr = concat . map show . tail . wordLengthes

-- | 04. 元素記号
--
-- Hi He Lied Because Boron Could Not Oxidize Fluorine.
-- New Nations Might Also Sign Peace Security Clause.
-- Arthur King Can.
--
-- という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は
-- 先頭の1文字，それ以外の単語は先頭に2文字を取り出し，
-- 取り出した文字列から単語の位置（先頭から何番目の単語か）への
-- 連想配列（辞書型もしくはマップ型）を作成せよ．
--
knock04 :: String -> [(Int, String)]
knock04 = map trimAtomWord . indexWordTuples
  where
    indexWordTuples :: String -> [(Int, String)]
    indexWordTuples s = map (\x -> (x, wordList!!(x-1))) [1 .. wordNum]
      where
        wordList = splitWords s
        wordNum = length wordList
    trimAtomWord :: (Int, String) -> (Int, String)
    trimAtomWord (1, s) = (1, [head s])
    trimAtomWord (5, s) = (5, [head s])
    trimAtomWord (6, s) = (6, [head s])
    trimAtomWord (7, s) = (7, [head s])
    trimAtomWord (9, s) = (9, [head s])
    trimAtomWord (15,s) = (15,[head s])
    trimAtomWord (16,s) = (16,[head s])
    trimAtomWord (n, s) = (n, [head s] ++ [(head . tail) s])

-- | 05. n-gram
--
-- 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
-- この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．
--
knock05 :: [a] -> [[a]]
knock05 (x1:x2:[]) = [[x1]++[x2]]
knock05 (x1:x2:xs) = [[x1]++[x2]] ++ (knock05 $ [x2]++xs)
knock05 _ = [[]]

-- | 06. 集合
--
-- "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，
-- それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．
-- さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．
--
knock06union :: String -> String -> [String]
knock06union s1 s2 = (nub . knock05) s1 `union` (nub . knock05) s2
knock06intersect :: String -> String -> [String]
knock06intersect s1 s2 = (nub . knock05) s1 `intersect` (nub . knock05) s2
knock06difference :: String -> String -> [String]
knock06difference s1 s2 = (nub . knock05) s1 \\ (nub . knock05) s2
knock06contains :: String -> String -> Bool
knock06contains s bi = (length $ filter (==bi) $ knock05 s) > 0

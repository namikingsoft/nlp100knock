module NLP100.Chapter02 where

import Text.Parsec
import Debug.Trace

-- | 10. 行数のカウント
--
-- 行数をカウントせよ．確認にはwcコマンドを用いよ．
--
knock10 :: FilePath -> IO Int
knock10 path = do
  rows <- lines <$> readFile path
  return $ length rows

-- | 11. タブをスペースに置換
--
-- タブ1文字につきスペース1文字に置換せよ．
-- 確認にはsedコマンド，trコマンド，もしくはexpandコマンドを用いよ．
--
knock11 :: FilePath -> IO String
knock11 path = do
  text <- readFile path
  return $ tab2space text
    where
      tab2space :: String -> String
      tab2space [] = ""
      tab2space ('\t':xs) = [' '] ++ tab2space xs
      tab2space (x:xs) = [x] ++ tab2space xs

-- | 12. 1列目をcol1.txtに，2列目をcol2.txtに保存
--
-- 各行の1列目だけを抜き出したものをcol1.txtに，
-- 2列目だけを抜き出したものをcol2.txtとしてファイルに保存せよ．
-- 確認にはcutコマンドを用いよ．
--
knock12 :: FilePath -> IO String
knock12 path = do
  text <- readFile path
  let lines = parseTsv text
  let file1 = foldl (\x y -> x ++ y ++ "\n") "" $ map (\x -> x!!0) lines
  let file2 = foldl (\x y -> x ++ y ++ "\n") "" $ map (\x -> x!!1) lines
  writeFile "/tmp/col1.txt" file1
  writeFile "/tmp/col2.txt" file2
  return  ""
    where
      parseTsv :: String -> [[String]]
      parseTsv x = case (parse tsv "error" x) of
        Right x -> x
        Left x -> [[]]
        where
          tsv = endBy line break
          line = sepBy col tab
          col = many1 $ noneOf "\t\n"
          tab = char '\t'
          break = char '\n'


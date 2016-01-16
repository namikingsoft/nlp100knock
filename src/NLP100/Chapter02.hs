module NLP100.Chapter02 where

import Data.List.Split (splitOn)
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
knock12 :: FilePath -> FilePath -> FilePath -> IO ()
knock12 path col1path col2path = do
  text <- readFile path
  let lines = parseTsv text
      col1text = init $ foldl (\x y -> x ++ y ++ "\n") "" $ map (\x -> x!!0) lines
      col2text = init $ foldl (\x y -> x ++ y ++ "\n") "" $ map (\x -> x!!1) lines
  writeFile col1path col1text
  writeFile col2path col2text
    where
      parseTsv :: String -> [[String]]
      parseTsv x = case (parse tsv "Parse Error" x) of
        Right x -> x
        Left x -> error $ show x
        where
          tsv = endBy line break
          line = sepBy col tab
          col = many1 $ noneOf "\t\n"
          tab = char '\t'
          break = char '\n'

-- | 13. col1.txtとcol2.txtをマージ
--
-- 12で作ったcol1.txtとcol2.txtを結合し，元のファイルの
-- 1列目と2列目をタブ区切りで並べたテキストファイルを作成せよ．
-- 確認にはpasteコマンドを用いよ．
knock13 :: FilePath -> FilePath -> FilePath -> IO ()
knock13 path col1path col2path = do
  col1text <- readFile col1path
  col2text <- readFile col2path
  let col1rows = splitBreak col1text
      col2rows = splitBreak col2text
      indexes = [0 .. (length col1rows - 1)]
      mergeRows = map (\x -> col1rows!!x ++ "\t" ++ col2rows!!x) indexes
      mergeText = init $ foldl (\x y -> x ++ y ++ "\n") "" mergeRows
  writeFile path mergeText
    where
      splitBreak = splitOn "\n"

-- | 14. 先頭からN行を出力
--
-- 自然数Nをコマンドライン引数などの手段で受け取り，
-- 入力のうち先頭のN行だけを表示せよ．確認にはheadコマンドを用いよ．
--
knock14 :: FilePath -> Int -> IO String
knock14 path n = do
  text <- readFile path
  return $ init $ foldl (\x y -> x ++ y ++ "\n") "" $ take n $ splitBreak text
    where
      splitBreak = splitOn "\n"

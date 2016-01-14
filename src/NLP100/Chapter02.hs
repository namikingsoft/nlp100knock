module NLP100.Chapter02 where

import Text.Regex

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
      tab2space x = subRegex (mkRegex "[\t]") x " "

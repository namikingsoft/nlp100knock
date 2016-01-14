module NLP100.Chapter02 where

-- | 10. 行数のカウント
--
-- 行数をカウントせよ．確認にはwcコマンドを用いよ．
--
knock10 :: FilePath -> IO Int
knock10 path = do
  rows <- lines <$> readFile path
  return $ length rows

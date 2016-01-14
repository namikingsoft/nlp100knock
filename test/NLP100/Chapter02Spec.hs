module NLP100.Chapter02Spec where

import NLP100.Chapter02
import Test.Hspec
import System.Process

exec :: String -> IO String
exec cmd = do
  result <- readProcess "sh" ["-c", cmd] []
  return $ rstrip result
    where
      lstrip :: String -> String
      lstrip [] = []
      lstrip xs'@(x:xs)
        | x == ' '  = lstrip xs
        | x == '\n' = lstrip xs
        | otherwise = xs'
      rstrip = reverse . lstrip . reverse
      strip = lstrip . rstrip

spec :: Spec
spec = do

  describe "第2章: UNIXコマンドの基礎" $ do
    -- hightemp.txtは，日本の最高気温の記録を「都道府県」「地点」「℃」「日」の
    -- タブ区切り形式で格納したファイルである．以下の処理を行うプログラムを作成し，
    -- hightemp.txtを入力ファイルとして実行せよ．さらに，
    -- 同様の処理をUNIXコマンドでも実行し，プログラムの実行結果を確認せよ．

    describe "10. 行数のカウント" $ do
      -- 行数をカウントせよ．確認にはwcコマンドを用いよ．
      it "should return correct value" $ do
        result1 <- knock10 "data/hightemp.txt"
        result2 <- exec "wc data/hightemp.txt | awk '{print $1}'"
        show result1 `shouldBe` result2

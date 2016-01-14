module NLP100.Chapter02Spec where

import NLP100.Chapter02
import Test.Hspec
import System.Process

exec :: String -> IO String
exec cmd = readProcess "sh" ["-c", cmd] []

exectrim :: String -> IO String
exectrim cmd = do
  result <- exec cmd
  return $ trim result
    where
      ltrim :: String -> String
      ltrim [] = []
      ltrim xs'@(x:xs)
        | x == ' '  = ltrim xs
        | x == '\n' = ltrim xs
        | otherwise = xs'
      rtrim = reverse . ltrim . reverse
      trim = ltrim . rtrim

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
        result2 <- exectrim "wc data/hightemp.txt | awk '{print $1}'"
        show result1 `shouldBe` result2

    describe "11. タブをスペースに置換" $ do
      -- タブ1文字につきスペース1文字に置換せよ．
      -- 確認にはsedコマンド，trコマンド，もしくはexpandコマンドを用いよ．
      it "should return correct value" $ do
        result1 <- knock11 "data/hightemp.txt"
        result2 <- exec "cat data/hightemp.txt | tr '\t' ' '"
        result1 `shouldBe` result2

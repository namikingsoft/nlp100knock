module NLP100.Chapter02Spec where

import NLP100.Chapter02
import Test.Hspec
import System.Process
import Debug.Trace

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

    describe "12. 1列目をcol1.txtに，2列目をcol2.txtに保存" $ do
      -- 各行の1列目だけを抜き出したものをcol1.txtに，
      -- 2列目だけを抜き出したものをcol2.txtとしてファイルに保存せよ．
      -- 確認にはcutコマンドを用いよ．
      it "should return correct value" $ do
        knock12 "data/hightemp.txt" "/tmp/col1.txt" "/tmp/col2.txt"
        result1 <- readFile "/tmp/col1.txt"
        result2 <- readFile "/tmp/col2.txt"
        col1 <- exectrim "cut -f1 data/hightemp.txt"
        col2 <- exectrim "cut -f2 data/hightemp.txt"
        result1 `shouldBe` col1
        result2 `shouldBe` col2

    describe "13. col1.txtとcol2.txtをマージ" $ do
      -- 12で作ったcol1.txtとcol2.txtを結合し，元のファイルの
      -- 1列目と2列目をタブ区切りで並べたテキストファイルを作成せよ．
      -- 確認にはpasteコマンドを用いよ．
      it "should return correct value" $ do
        knock13 "/tmp/merge.txt" "/tmp/col1.txt" "/tmp/col2.txt"
        result <- readFile "/tmp/merge.txt"
        answer <- exectrim "paste -d'\t' /tmp/col1.txt /tmp/col2.txt"
        result `shouldBe` answer

    describe "14. 先頭からN行を出力" $ do
      -- 自然数Nをコマンドライン引数などの手段で受け取り，
      -- 入力のうち先頭のN行だけを表示せよ．確認にはheadコマンドを用いよ．
      it "should return correct value" $ do
        result1 <- knock14 "data/hightemp.txt" 4
        answer1 <- exectrim "head -n4 data/hightemp.txt"
        result1 `shouldBe` answer1
        result2 <- knock14 "data/hightemp.txt" 5
        answer2 <- exectrim "head -n5 data/hightemp.txt"
        result2 `shouldBe` answer2

    describe "15. 末尾のN行を出力" $ do
      -- 自然数Nをコマンドライン引数などの手段で受け取り，
      -- 入力のうち末尾のN行だけを表示せよ．確認にはtailコマンドを用いよ．
      it "should return correct value" $ do
        result1 <- knock15 "data/hightemp.txt" 4
        answer1 <- exectrim "tail -n4 data/hightemp.txt"
        result1 `shouldBe` answer1
        result2 <- knock15 "data/hightemp.txt" 5
        answer2 <- exectrim "tail -n5 data/hightemp.txt"
        result2 `shouldBe` answer2

module NLP100.Chapter01Spec where

import NLP100.Chapter01
import NLP100.Chapter01.Internal
import Test.Hspec

spec :: Spec
spec = do

  describe "第1章: 準備運動" $ do

    describe "00. 文字列の逆順" $ do
      -- 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．
      it "should return correct value" $ do
        knock00 "stressed" `shouldBe` "desserts"

    describe "01. 「パタトクカシーー」" $ do
      -- 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して
      -- 連結した文字列を得よ．
      it "should return correct value" $ do
        knock01 "パタトクカシーー" `shouldBe` "パトカー"

    describe "02. 「パトカー」＋「タクシー」＝「パタトクカシーー」" $ do
      -- パトカー」＋「タクシー」の文字を先頭から交互に連結して
      -- 文字列「パタトクカシーー」を得よ．
      it "should return correct value" $ do
        knock02 "パトカー" "タクシー" `shouldBe` "パタトクカシーー"

    describe "03. 円周率" $ do
      -- Now I need a drink, alcoholic of course,
      -- after the heavy lectures involving quantum mechanics.
      --
      -- という文を単語に分解し，各単語の（アルファベットの）文字数を
      -- 先頭から出現順に並べたリストを作成せよ．
      it "should return correct value" $ do
        let
          row1 = "Now I need a drink, alcoholic of course, "
          row2 = "after the heavy lectures involving quantum mechanics."
          sentence = row1 ++ row2
        knock03 sentence `shouldBe` "3.14159265358979"

    describe "04. 元素記号" $ do
      -- Hi He Lied Because Boron Could Not Oxidize Fluorine.
      -- New Nations Might Also Sign Peace Security Clause.
      -- Arthur King Can.
      --
      -- という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は
      -- 先頭の1文字，それ以外の単語は先頭に2文字を取り出し，
      -- 取り出した文字列から単語の位置（先頭から何番目の単語か）への
      -- 連想配列（辞書型もしくはマップ型）を作成せよ．
      it "should return correct value" $ do
        let
          row1 = "Hi He Lied Because Boron Could Not Oxidize Fluorine. "
          row2 = "New Nations Might Also Sign Peace Security Clause. "
          row3 = "Arthur King Can."
          sentence = row1 ++ row2 ++ row3
        knock04 sentence `shouldBe` [
          (1,"H"),(2,"He"),(3,"Li"),(4,"Be"),(5,"B"),(6,"C"),(7,"N"),
          (8,"Ox"),(9,"F"),(10,"Ne"),(11,"Na"),(12,"Mi"),(13,"Al"),(14,"Si"),
          (15,"P"),(16,"S"),(17,"Cl"),(18,"Ar"),(19,"Ki"),(20,"Ca")
          ]

    describe "05. n-gram" $ do
      -- 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
      -- この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．
      it "should return correct value" $ do
        let
          text = "I am an NLPer"
          words = splitWords text
        knock05 text `shouldBe` [
          "I "," a", "am","m "," a","an","n "," N","NL","LP","Pe","er"
          ]
        knock05 words `shouldBe` [
          ["I","am"],["am","an"],["an","NLPer"]
          ]

    describe "06. 集合" $ do
      -- "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，
      -- それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．
      -- さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．
      it "should return correct value" $ do
        let
          x = "paraparaparadise"
          y = "paragraph"
          k = "se"
        knock06union x y `shouldBe` [
          "pa","ar","ra","ap","ad","di","is","se","ag","gr","ph"
          ]
        knock06intersect x y `shouldBe` [
          "pa","ar","ra","ap"
          ]
        knock06difference x y `shouldBe` [
          "ad","di","is","se"
          ]
        knock06contains x k `shouldBe` True
        knock06contains y k `shouldBe` False

    describe "07. テンプレートによる文生成" $ do
      -- 引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．
      -- さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．
      it "should return correct value" $ do
        knock07 12 "気温" 22.4 `shouldBe` "12時の気温は22.4"

    describe "08. 暗号文" $ do
      -- 与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
      --
      -- ・英小文字ならば(219 - 文字コード)の文字に置換
      -- ・その他の文字はそのまま出力
      --
      -- この関数を用い，英語のメッセージを暗号化・復号化せよ．
      it "should return correct value" $ do
        knock08encode "Test08" `shouldBe` "Tvhg08"
        knock08decode "Tvhg08" `shouldBe` "Test08"

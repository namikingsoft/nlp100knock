module NLP100.Chapter01Spec where

import NLP100.Chapter01
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
        knock03 sentence `shouldBe` "3.14159265358979"
          where sentence = row1 ++ row2
                row1 = "Now I need a drink, alcoholic of course, "
                row2 = "after the heavy lectures involving quantum mechanics."

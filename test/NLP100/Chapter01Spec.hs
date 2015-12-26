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

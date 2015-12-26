module NLP100.Chapter01Spec where

import NLP100.Chapter01
import Test.Hspec

spec :: Spec
spec = do

  describe "第1章: 準備運動" $ do

    describe "00. 文字列の逆順" $ do

      it "should return correct value" $ do
        knock00 "stressed" `shouldBe` "desserts"

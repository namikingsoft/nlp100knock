module My.FibSpec where

import My.Fib
import Test.Hspec

spec :: Spec
spec = do

  describe "My.Fib" $ do

    describe "fib" $ do

      it "should return fibonacci number" $ do
        fib 10 `shouldBe` 55
        fib 5 `shouldBe` 5

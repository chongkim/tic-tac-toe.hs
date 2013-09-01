import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Game

main = hspec $ do
  describe "TTT.Game" $ do
    context "askWhoGoesFirst" $ do
      it "should ask who goes first" $ do
        askWhoGoesFirst `shouldBe` (return Human)

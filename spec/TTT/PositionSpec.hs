import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "Position" $ do
    describe "new" $ do
      it "should create an initial position" $ do
        (board new) `shouldBe` "   "
                            ++ "   "
                            ++ "   "

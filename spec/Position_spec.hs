import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Position

main = hspec $ do
  describe "new" $ do
    it "should create an initial position" $ do
      new `shouldBe` Position ("   "
                            ++ "   "
                            ++ "   ") 'X'
  describe "show" $ do
    it "should display the board" $ do
      show new `shouldBe` "   |   |   \n"
                       ++ "-----------\n"
                       ++ "   |   |   \n"
                       ++ "-----------\n"
                       ++ "   |   |   \n"

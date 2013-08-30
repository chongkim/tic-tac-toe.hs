import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "Position" $ do
    describe "new" $ do
      it "should get an initial board position" $ do
        (board new) `shouldBe` "   "
                            ++ "   "
                            ++ "   "
      it "should get be X's turn" $ do
        (turn new) `shouldBe` 'X'
    describe "render" $ do
      it "should render the board" $ do
        (render new) `shouldBe` " 0 | 1 | 2 \n"
                             ++ "-----------\n"
                             ++ " 3 | 4 | 5 \n"
                             ++ "-----------\n"
                             ++ " 6 | 7 | 8 \n"
    describe "move" $ do
      it "should make a move" $ do
        (board (move new 0)) `shouldBe` "X  "
                                       ++ "   "
                                       ++ "   "


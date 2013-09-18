import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "TTT.Position" $ do
    context "initPosition" $ do
      it "should create a new position" $ do
        initPosition `shouldBe` (Position ("   "++"   "++"   ") 'X')
    context "move" $ do
      it "should make a move" $ do
        initPosition `move` 0 `shouldBe` (Position ("X  "++"   "++"   ") 'O')
    context "possibleMoves" $ do
      it "should list possible moves for a position" $ do
        possibleMoves (Position ("XX "++" OO"++"   ") 'X') `shouldBe` [2,3,6,7,8]
    context "isWinFor" $ do
      it "should determine no win" $ do
        initPosition `isWinFor` 'X' `shouldBe` False
      it "should determine a win for X in first row" $ do
        (Position ("XXX"++"   "++"   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for X in second row" $ do
        (Position ("   "++"XXX"++"   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for X in third row" $ do
        (Position ("   "++"   "++"XXX") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for O in first row" $ do
        (Position ("O  "++
                   "O  "++
                   "O  ") 'O') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O in second row" $ do
        (Position (" O "++
                   " O "++
                   " O ") 'O') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O in third row" $ do
        (Position ("  O"++
                   "  O"++
                   "  O") 'O') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O in major diagonal" $ do
        (Position ("O  "++
                   " O "++
                   "  O") 'O') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O in minor diagonal" $ do
        (Position ("  O"++
                   " O "++
                   "O  ") 'O') `isWinFor` 'O' `shouldBe` True

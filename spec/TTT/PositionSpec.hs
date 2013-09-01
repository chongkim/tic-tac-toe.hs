import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "TTT.Position" $ do
    context "initPosition" $ do
      it "should create an initial position" $ do
        initPosition `shouldBe` Position ("   "
                                        ++"   "
                                        ++"   ") 'X'
    context "render" $ do
      it "should render a position" $ do
        render (Position (" X "
                        ++"XO "
                        ++"   ") 'O') `shouldBe` " 0 | X | 2 \n"
                                              ++ "-----------\n"
                                              ++ " X | O | 5 \n"
                                              ++ "-----------\n"
                                              ++ " 6 | 7 | 8 \n"
    context "move" $ do
      it "should make a move" $ do
        initPosition `move` 3 `shouldBe`
          (Position ("   "
                  ++ "X  "
                  ++ "   ") 'O')
      it "should make two moves" $ do
        initPosition `move` 3 `move` 0 `shouldBe`
          (Position ("O  "
                  ++ "X  "
                  ++ "   ") 'X')
    context "possibleMoves" $ do
      it "should find possible moves for a position" $ do
        possibleMoves (Position (" X "
                              ++ "OX "
                              ++ "   ") 'O') `shouldBe` [0,2,5,6,7,8]
    context "possiblePositions" $ do
      it "should find next possible positions from the given position" $ do
        possiblePositions initPosition `shouldSatisfy`
          ((Position ("X  "
                   ++ "   "
                   ++ "   ") 'O') `elem`)
        possiblePositions initPosition `shouldSatisfy`
          ((Position ("   "
                   ++ " X "
                   ++ "   ") 'O') `elem`)
    context "isWinFor" $ do
      it "should determine no win" $ do
        initPosition `isWinFor` 'X' `shouldBe` False
      it "should determine win for X on first row" $ do
        (Position ("XXX"
                ++ "   "
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for X on second row" $ do
        (Position ("   "
                ++ "XXX"
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for X on third row" $ do
        (Position ("   "
                ++ "   "
                ++ "XXX") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for O on first col" $ do
        (Position ("O  "
                ++ "O  "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine win for O on second col" $ do
        (Position (" O "
                ++ " O "
                ++ " O ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine win for O on third col" $ do
        (Position ("  O"
                ++ "  O"
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine win for O on major diagonal" $ do
        (Position ("O  "
                ++ " O "
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine win for O on minor diagonal" $ do
        (Position ("  O"
                ++ " O "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
    context "spaces" $ do
      it "should count the spaces in a board" $ do
        spaces (Position "X        " 'O') `shouldBe` 8
    context "minimax" $ do
      it "should determine a win for X" $ do
        minimax (Position "XXX      " 'O') `shouldBe` 100
      it "should determine a win for O" $ do
        minimax (Position "OOO      " 'X') `shouldBe` (-100)
      it "should determine a draw" $ do
        minimax (Position ("OXO"
                        ++ "XOX"
                        ++ "XOX") 'X') `shouldBe` 0
      it "should determine a win for X in 1 move" $ do
        minimax (Position "XX       " 'X') `shouldBe` 107
      it "should determine a win for O in 1 move" $ do
        minimax (Position "OO       " 'O') `shouldBe` (-107)
    context "bestMove" $ do
      it "should find the win for X" $ do
        bestMove (Position ("XX "
                          ++"   "
                          ++"   ") 'X') `shouldBe` [] --2

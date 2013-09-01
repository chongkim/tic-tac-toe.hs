import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "TTT.Position" $ do
    context "initPosition" $ do
      it "should create an blank position" $ do
        initPosition `shouldBe` Position ("   "
                                        ++"   "
                                        ++"   ") 'X'
    context "render" $ do
      it "should render the board" $ do
        render (Position (" X "
                        ++"OO "
                        ++"   ") 'X') `shouldBe` " 0 | X | 2 \n"
                                              ++ "-----------\n"
                                              ++ " O | O | 5 \n"
                                              ++ "-----------\n"
                                              ++ " 6 | 7 | 8 \n"
    context "move" $ do
      it "should make a move" $ do
        initPosition `move` 0 `shouldBe` Position "X        " 'O'
      it "should make two moves" $ do
        initPosition `move` 3 `move` 5 `shouldBe`
          Position ("   "
                  ++"X O"
                  ++"   ") 'X'
    context "possibleMoves" $ do
      it "should find possible moves given a position" $ do
        possibleMoves (Position ("  X"
                               ++"OX "
                               ++"   ") 'O') `shouldBe` [0,1,5,6,7,8]
    context "possiblePositions" $ do
      it "should find possible positions for next move" $ do
        possiblePositions (Position ("  X"
                                   ++"OX "
                                   ++"   ") 'O') `shouldSatisfy`
          ((Position ("O X"
                    ++"OX "
                    ++"   ") 'X') `elem`)
    context "isWinFor" $ do
      it "should determine no win" $ do
        initPosition `isWinFor` 'X' `shouldBe` False
      it "should determine a win for X on first row" $ do
        (Position ("XXX"
                ++ "   "
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for X on second row" $ do
        (Position ("   "
                ++ "XXX"
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for X on third row" $ do
        (Position ("   "
                ++ "   "
                ++ "XXX") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine a win for O on first col" $ do
        (Position ("O  "
                ++ "O  "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O on second col" $ do
        (Position (" O "
                ++ " O "
                ++ " O ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O on third col" $ do
        (Position ("  O"
                ++ "  O"
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O on major diagonal" $ do
        (Position ("O  "
                ++ " O "
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine a win for O on minor diagonal" $ do
        (Position ("  O"
                ++ " O "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
    context "minimax" $ do
      it "should determine a win for X" $ do
        minimax (Position "XXX      " 'O') `shouldBe` 100
      it "should determine a win for O" $ do
        minimax (Position "OOO      " 'O') `shouldBe` (-100)
      it "should determine a draw" $ do
        minimax(Position ("XOX"
                        ++"OXO"
                        ++"OXO") 'X') `shouldBe` 0
      it "should determine a win in 1 move for X" $ do
        minimax (Position "XX       " 'X') `shouldBe` 93
      it "should determine a win in 1 move for O" $ do
        minimax (Position "OO       " 'O') `shouldBe` (-93)
    -- context "bestMove" $ do
    --   it "should find the best move for win in 1" $ do
    --     bestMove (Position ("XX "
    --                       ++"   "
    --                       ++"   ") 'X') `shouldBe` 2

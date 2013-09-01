import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "TTT.Position" $ do
    context "initPosition" $ do
      it "should create an initial position" $ do
        initPosition `shouldBe` Position ("   "
                                       ++ "   "
                                       ++ "   ") 'X'
    context "render" $ do
      it "should render a position" $ do
        render (Position (" X "
                        ++"OO "
                        ++"   ") 'X') `shouldBe` " 0 | X | 2 \n"
                                              ++ "-----------\n"
                                              ++ " O | O | 5 \n"
                                              ++ "-----------\n"
                                              ++ " 6 | 7 | 8 \n"
    context "move" $ do
      it "should make a move" $ do
        initPosition `move` 0
          `shouldBe` Position ("X  "
                            ++ "   "
                            ++ "   ") 'O'
      it "should make two moves" $ do
        initPosition `move` 0 `move` 2
          `shouldBe` Position ("X O"
                            ++ "   "
                            ++ "   ") 'X'
    context "possibleMoves" $ do
      it "should list possible moves for a position" $ do
        possibleMoves (Position (" X "
                               ++" XO"
                               ++"O  ") 'X') `shouldBe` [0,2,3,7,8]

    context "isWinFor" $ do
      it "should determine no win for X" $ do
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
      it "should determine win for X on first col" $ do
        (Position ("X  "
                ++ "X  "
                ++ "X  ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for X on second col" $ do
        (Position (" X "
                ++ " X "
                ++ " X ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for X on third col" $ do
        (Position ("  X"
                ++ "  X"
                ++ "  X") 'X') `isWinFor` 'X' `shouldBe` True
      it "should determine win for O on major diagonal" $ do
        (Position ("O  "
                ++ " O "
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should determine win for O on minor diagonal" $ do
        (Position ("  O"
                ++ " O "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
    context "evalLeaf" $ do
      it "should evaluate Nothing for undetermined win" $ do
        evalLeaf initPosition `shouldBe` (Nothing :: Maybe Int)
      it "should evaluate Just 100 for win for X" $ do
        evalLeaf (Position ("XXX      ") 'X') `shouldBe` Just 100
      it "should evaluate Just -100 for win for O" $ do
        evalLeaf (Position ("OOO      ") 'X') `shouldBe` Just (-100)
      it "should evaluate Just 0 for no win" $ do
        evalLeaf (Position ("OXO"
                          ++"XOX"
                          ++"XOX") 'O') `shouldBe` Just 0
    context "minimax" $ do
      it "should evaluate a won position for X" $ do
        minimax (Position "XXX      " 'O') `shouldBe` 100
      it "should evaluate a won position for O" $ do
        minimax (Position "OOO      " 'O') `shouldBe` (-100)
      it "should evaluate a win in 1 for X" $ do
        minimax (Position "XX       " 'O') `shouldBe` 99
      it "should evaluate a win in 1 for O" $ do
        minimax (Position "OO       " 'X') `shouldBe` (-99)

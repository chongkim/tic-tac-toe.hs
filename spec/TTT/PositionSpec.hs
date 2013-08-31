import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "Position" $ do
    context "initPosition" $ do
      it "should set up the initial board" $ do
        initPosition `shouldBe` Position ("   "
                                       ++ "   "
                                       ++ "   ") 'X'
    context "render" $ do
      it "should render the board" $ do
        (render initPosition) `shouldBe` " 0 | 1 | 2 \n"
                                      ++ "-----------\n"
                                      ++ " 3 | 4 | 5 \n"
                                      ++ "-----------\n"
                                      ++ " 6 | 7 | 8 \n"
    context "move" $ do
      it "should make a move" $ do
        initPosition `move` 0 `shouldBe` Position ("X  "
                                                ++ "   "
                                                ++ "   ") 'O'
    context "possibleMovesFor" $ do
      it "should get possible moves for initial position" $ do
        possibleMovesFor initPosition `shouldBe` [0..8]
      it "should get possible moves for a position" $ do
        (possibleMovesFor . (`move` 0) . (`move` 3)) initPosition `shouldBe`
          [1,2,4,5,6,7,8]
    context "isWinFor" $ do
      it "should be a win for X from initial position" $ do
        initPosition `isWinFor` 'X' `shouldBe` False
      it "should be a win for X on first row" $ do
        (Position ("XXX"
                ++ "   "
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should be a win for X on second row" $ do
        (Position ("   "
                ++ "XXX"
                ++ "   ") 'X') `isWinFor` 'X' `shouldBe` True
      it "should be a win for O on third row" $ do
        (Position ("   "
                ++ "   "
                ++ "OOO") 'X') `isWinFor` 'O' `shouldBe` True
      it "should be a win for O on first col" $ do
        (Position ("O  "
                ++ "O  "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should be a win for O on second col" $ do
        (Position (" O "
                ++ " O "
                ++ " O ") 'X') `isWinFor` 'O' `shouldBe` True
      it "should be a win for O on third col" $ do
        (Position ("  O"
                ++ "  O"
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should be a win for O on major diagonal" $ do
        (Position ("O  "
                ++ " O "
                ++ "  O") 'X') `isWinFor` 'O' `shouldBe` True
      it "should be a win for O on minor diagonal" $ do
        (Position ("  O"
                ++ " O "
                ++ "O  ") 'X') `isWinFor` 'O' `shouldBe` True
    context "evalLeaf" $ do
      it "should not find an evaluation for an incomplete position" $ do
        evalLeaf initPosition `shouldBe` (Nothing :: Maybe Int)
      it "should evaluate a win for X" $ do
        evalLeaf (Position ("XXX"
                          ++"   "
                          ++"   ") 'O') `shouldBe` Just 100
      it "should evaluate a win for O" $ do
        evalLeaf (Position ("   "
                          ++"OOO"
                          ++"   ") 'O') `shouldBe` Just (-100)
      it "should evaluate a completed non-win position" $ do
        evalLeaf (Position ("OXO"
                          ++"OXO"
                          ++"XOX") 'X') `shouldBe` Just 0
    context "minimax" $ do
      it "should evaluate an already won position" $ do
        minimax (Position ("XXX"
                        ++ "   "
                        ++ "   ") 'O') `shouldBe` 100

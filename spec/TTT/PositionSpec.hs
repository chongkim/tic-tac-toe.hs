import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Position

main = hspec $ do
  describe "Position" $ do
    context "new" $ do
      it "should create a new board position" $ do
        new `shouldBe` (Position ("   "
                               ++ "   "
                               ++ "   ") 'X')
    context "render" $ do
      it "should print out the initial board" $ do
        (render new) `shouldBe` " 0 | 1 | 2 \n"
                             ++ "-----------\n"
                             ++ " 3 | 4 | 5 \n"
                             ++ "-----------\n"
                             ++ " 6 | 7 | 8 \n"
      it "should print out the board with pieces" $ do
        (render (Position ("X  "
                        ++ " XO"
                        ++ " O ") 'X')) `shouldBe` " X | 1 | 2 \n"
                                                ++ "-----------\n"
                                                ++ " 3 | X | O \n"
                                                ++ "-----------\n"
                                                ++ " 6 | O | 8 \n"
    context "move" $ do
      it "should place a piece on the board" $ do
        (new `move` 0) `shouldBe` (Position ("X  "
                                          ++ "   "
                                          ++ "   ") 'O')
      it "should place a piece on the board" $ do
        (new `move` 2) `shouldBe` (Position ("  X"
                                          ++ "   "
                                          ++ "   ") 'O')
    context "possibleMoves" $ do
      it "should list possible moves of initial position" $ do
        possibleMoves new `shouldBe` [0..8]
      it "should list possible moves after a move" $ do
        (possibleMoves . (`move` 3)) new `shouldBe` [0,1,2,4,5,6,7,8]
    context "isWinFor" $ do
      it "should not find a win for initial position" $ do
        (new `isWinFor` 'X') `shouldBe` False
      it "should be a win for X on first row" $ do
        ((Position ("XXX"
                 ++ "   "
                 ++ "   ") 'O') `isWinFor` 'X') `shouldBe` True
      it "should be a win for O on second row" $ do
        ((Position ("   "
                 ++ "OOO"
                 ++ "   ") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on third row" $ do
        ((Position ("   "
                 ++ "   "
                 ++ "OOO") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on first col" $ do
        ((Position ("O  "
                 ++ "O  "
                 ++ "O  ") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on second col" $ do
        ((Position (" O "
                 ++ " O "
                 ++ " O ") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on third col" $ do
        ((Position ("  O"
                 ++ "  O"
                 ++ "  O") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on major diagonal" $ do
        ((Position ("O  "
                 ++ " O "
                 ++ "  O") 'O') `isWinFor` 'O') `shouldBe` True
      it "should be a win for O on minor diagonal" $ do
        ((Position ("  O"
                 ++ " O "
                 ++ "O  ") 'O') `isWinFor` 'O') `shouldBe` True
    context "evalLeaf" $ do
      it "should evaluate a non-win position" $ do
        (evalLeaf new) `shouldBe` (Nothing :: Maybe Int)

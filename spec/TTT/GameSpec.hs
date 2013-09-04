import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Game
import TTT.Position
import Control.Monad.Fix

main = hspec $ do
  describe "TTT.Game" $ do
    context "askForPlayer" $ do
      it "should answer Human" $ do
        ans <- askForPlayer (return "2")
        ans `shouldBe` Human
      it "should answer Computer" $ do
        ans <- askForPlayer (return "1")
        ans `shouldBe` Computer
    context "askForMove" $ do
      it "should quit" $ do
        position <- return initPosition
        ans <- askForMove (return "q") position
        ans `shouldBe` 9
      it "should pick a valid move" $ do
        position <- return initPosition
        ans <- askForMove (return "1") position
        ans `shouldBe` 0
    context "askToPlayAgain" $ do
      it "should prompt to play again" $ do
        ans <- askToPlayAgain (return "y")
        ans `shouldBe` True
      it "should prompt to play again" $ do
        ans <- askToPlayAgain (return "n")
        ans `shouldBe` False


import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Game
import TTT.Position
import Control.Monad.State

getLineStub = do
  GameState fn (x:xs) <- get
  put (GameState fn xs)
  return x

main = hspec $ do
  describe "TTT.Game" $ do
    context "render" $ do
      it "should render a position" $ do
        render (Position ("XX "++" OO"++"   ") 'X')
          `shouldBe` " X | X | 2 \n"++
                     "-----------\n"++
                     " 3 | O | O \n"++
                     "-----------\n"++
                     " 6 | 7 | 8 \n"
    context "askForPlayer" $ do
      it "should quit" $ do
        result <- evalStateT askForPlayer (GameState getLineStub ["q"])
        result `shouldBe` Nothing
      it "should pick the computer" $ do
        result <- evalStateT askForPlayer (GameState getLineStub ["1","q"])
        result `shouldBe` Just Computer
      it "should pick the human" $ do
        result <- evalStateT askForPlayer (GameState getLineStub ["2"])
        result `shouldBe` Just Human
      it "should handle bad input" $ do
        result <- evalStateT askForPlayer (GameState getLineStub ["xx","2"])
        result `shouldBe` Just Human
    context "askForMove" $ do
      it "should quit" $ do
        result <- evalStateT (askForMove initPosition)
                              (GameState getLineStub ["q"])
        result `shouldBe` Nothing
      it "should make a valid move" $ do
        result <- evalStateT (askForMove initPosition)
                              (GameState getLineStub ["1"])
        result `shouldBe` Just 1
      it "should make a invalid move" $ do
        result <- evalStateT (askForMove (Position ("XX "++" OO"++"   ") 'X'))
                              (GameState getLineStub ["1", "2"])
        result `shouldBe` Just 2
      it "should handle bad input" $ do
        result <- evalStateT (askForMove initPosition)
                              (GameState getLineStub ["xxx","1"])
        result `shouldBe` Just 1
    context "play" $ do
      it "should quit" $ do
        result <- evalStateT play (GameState getLineStub ["q"])
        result `shouldBe` Quit
      it "should select the human to go first" $ do
        result <- evalStateT play (GameState getLineStub ["2", "q"])
        result `shouldBe` Quit
      it "should make a move" $ do
        result <- evalStateT play (GameState getLineStub ["2", "0", "q"])
        result `shouldBe` Quit
      it "should play a complete game" $ do
        result <- evalStateT play (GameState getLineStub ["2", "0", "1", "3", "q"])
        result `shouldBe` ComputerWin
      

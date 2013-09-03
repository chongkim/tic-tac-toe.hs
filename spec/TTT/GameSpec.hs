import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import TTT.Game

main = hspec $ do
  describe "TTT.Game" $ do
    it "should play" $ do
      play

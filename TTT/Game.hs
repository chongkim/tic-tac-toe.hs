module TTT.Game (
  Player (..)
 ,play
 ,askForPlayer
 ,askForMove
 ,askToPlayAgain
 ,displayWinner
 ,moveLoop
)  where

import TTT.Position
import System.Random

data Player = Computer | Human deriving (Show, Eq)

other :: Player -> Player
other Human = Computer
other Computer = Human

askForPlayer getLine = do
  putStrLn "Who do you want to move first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _   -> do
             putStrLn "Please choose 1 or 2"
             askForPlayer getLine

askForMove getLine position@(Position board _) = do
  putStrLn "Move (q - quit)"
  ans <- getLine
  if ans == "q" then
    return 9
  else if ans!!0 `elem` ['1'..'9'] then do
    idx <- return ((read ans :: Int) - 1)
    if (board!!idx == ' ') then
      return idx
    else do
      putStrLn "Please enter a valid move"
      askForMove getLine position
  else
    askForMove getLine position

askToPlayAgain getLine = do
  putStrLn "Do you want to play again? (y/n)"
  ans <- getLine
  case ans of
    "y" -> return True
    "n" -> return False
    _   -> do askToPlayAgain getLine

displayWinner position x o = do
  putStrLn (render position)
  if position `isWinFor` 'X' then do
    putStrLn ("Winner "++(show x))
  else if position `isWinFor` 'O' then do
    putStrLn ("Winner "++(show o))
  else do
    putStrLn ("Draw.")

moveLoop position player x o = do
  putStrLn (render position)
  idx <- if player == Computer then
           if isBlank position then do
             randomRIO (0,8)
           else
             return (bestMove position)
         else
           askForMove getLine position
  if idx == 9 then do
    return False
  else do
    position <- return (position `move` idx)
    if isEnd position then do
      displayWinner position x o
      return True
    else do
      moveLoop position (other player) x o

play getLine = do
  player <- askForPlayer getLine
  position <- return initPosition
  doPrompt <- moveLoop position player player (other player)
  if doPrompt then do
    isPlayAgain <- askToPlayAgain getLine
    if isPlayAgain then do
      play getLine
    else do
      putStrLn "Goodbye"
  else do
    putStrLn "Goodbye"

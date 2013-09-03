module TTT.Game (
  play
)  where

import TTT.Position
import System.Random

data Player = Computer | Human deriving (Show, Eq)

other :: Player -> Player
other Human = Computer
other Computer = Human

askForPlayer = do
  putStrLn "Who do you want to move first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _   -> askForPlayer

askForMove = do
  putStrLn "Move (q - quit)"
  ans <- getLine
  if ans == "q" then
    return 9
  else if ans!!0 `elem` ['0'..'8'] then
    return (read ans :: Int)
  else
    askForMove

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
           askForMove
  if idx == 9 then do
    putStrLn "Goodbye"
  else do
    position <- return (position `move` idx)
    if isEnd position then do
      displayWinner position x o
    else do
      moveLoop position (other player) x o

play = do
  player <- askForPlayer
  position <- return initPosition
  moveLoop position player player (other player)

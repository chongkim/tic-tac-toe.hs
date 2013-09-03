module TTT.Game (
  play
)  where

import TTT.Position
import System.Random

data Player = Computer | Human deriving (Show, Eq)

askForPlayer = do
  putStrLn "Who do you want to play first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _ -> askForPlayer

other :: Player -> Player
other Human = Computer
other Computer = Human

askForMove = do
  putStrLn "Move (q - to quit)"
  ans <- getLine
  if ans == "q" then
    return 9
  else if ((ans !! 0) `elem` ['0'..'8']) then
    return (read ans :: Int)
  else
    askForMove

moveLoop position player x o = do
  idx <- if player == Human then do
           askForMove
         else
           if isBlank position then do
             randomRIO (0,8)
           else
             return (bestMove position)
  if idx < 9 then do
    position <- return (position `move` idx)
    putStrLn (render position)
    if isEnd position then
      if position `isWinFor` 'X' then do
        putStrLn ("Winner: "++(show x))
      else if position `isWinFor` 'O' then do
        putStrLn ("Winner: "++(show o))
      else
        putStrLn "Draw."
    else
      moveLoop position (other player) x o
  else
    putStrLn "Goodbye!"

play = do
  player <- askForPlayer
  position <- return initPosition
  putStrLn (render position)
  moveLoop position player player (other player)
  putStrLn "done"

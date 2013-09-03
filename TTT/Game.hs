module TTT.Game (
  Player (..)
 ,play
)  where

import TTT.Position
import System.Random

data Player = Computer | Human deriving (Show, Eq)

other Computer = Human
other Human = Computer

askForPlayer = do
  putStrLn "Who do you want to play first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _ -> askForPlayer

askForMove position = do
  putStrLn "Move (or q to quit)"
  ans <- getLine
  if ans == "q" then
    return 9
  else if (ans !! 0) `elem` ['0'..'8'] then do
    return (read ans :: Int)
  else
    askForMove position

moveLoop position player x o = do
  idx <- if player == Human then do
            putStrLn (render position)
            askForMove position
          else
            if isBlank position then
              randomRIO (0,8)
            else
              return $ bestMove position
  if idx < 9 then do
    position <- return (position `move` idx)
    if isEnd position then do
      if position `isWinFor` 'X' then do
        putStrLn ((show x)++" wins")
      else if position `isWinFor` 'O' then do
        putStrLn ((show o)++" wins")
      else do
        putStrLn "draw"
      putStrLn (render position)
    else
      moveLoop position (other player) x o
  else do
    putStrLn "Goodbye"

play = do
  player <- askForPlayer
  moveLoop initPosition player player (other player)
  putStrLn "done"

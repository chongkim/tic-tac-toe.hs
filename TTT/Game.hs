module TTT.Game (
  Player (..)
 ,askForPlayer
 ,play
)  where

import TTT.Position
import System.Random

data Player = Computer | Human deriving (Show, Eq)

other :: Player -> Player
other Human = Computer
other Computer = Human

askForPlayer = do
  putStrLn "Who do you want to go first?"
  putStrLn "  1. Computer"
  putStrLn "  2. Human"
  ans <- getLine
  case ans of
    "1" -> return Computer
    "2" -> return Human
    _ -> askForPlayer

askForMove = do
  putStrLn "Make a move"
  ans <- getLine
  if (ans !! 0) `elem` ['0'..'8']
    then return (read ans :: Int)
    else askForMove

moveLoop position player = do
  idx <- if player == Human then do
           putStrLn (render position)
           askForMove
         else do
           return $ bestMove position
           -- if isBlank position then do
           --   randomRIO (0,8)
           -- else
           --   return $ bestMove position
  position <- return (position `move` idx)
  putStrLn (render position)
  if not (isEnd position) then do
    moveLoop position (other player)
  else do
    putStrLn "Thanks for Playing"

play = do
  player <- askForPlayer
  moveLoop initPosition player
  putStrLn "done"

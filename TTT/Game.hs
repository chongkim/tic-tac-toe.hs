module TTT.Game (
  Player (..)
 ,GameState (..)
 ,GameStatus (..)
 ,render
 ,askForPlayer
 ,askForMove
 ,play
)  where

import Control.Monad.State
import TTT.Position
import Data.Char
import Data.List.Split
import Data.List

import TTT.Engine
import Control.Monad.State
import qualified Data.Map as M

data Player = Computer | Human deriving (Show, Eq, Ord)
data GameState = GameState (StateT GameState IO String) [String]
data GameStatus = Quit | ComputerWin | HumanWin | Draw deriving (Show, Eq, Ord)

render (Position board turn) =
  unlines . intersperse (replicate (dim*4-1) '-') . map (intercalate "|") .
  chunksOf dim . map (pad . display) $ zip [0..] board
  where display (i,' ') = intToDigit i
        display (_, c) = c
        pad c = ' ':c:" "

getline = do
  GameState fn x <- get
  fn

askForPlayer = do
  lift $ putStrLn "Who do you want to go first?"
  lift $ putStrLn "  1. Computer"
  lift $ putStrLn "  2. Human"
  lift $ putStrLn "  q - quit"
  ans <- getline
  case ans of
    "q" -> return Nothing
    "1" -> return $ Just Computer
    "2" -> return $ Just Human
    _   -> askForPlayer

askForMove position@(Position board turn) = do
  lift $ putStrLn "Make a move"
  ans <- getline
  case ans of
    "q" -> return Nothing
    [c] | isDigit c || c `elem` "abcdef" -> do
      let idx = digitToInt c
      if 0 <= idx && idx < size && board !! idx == ' '
        then return $ Just idx
        else do
          lift $ putStrLn "Pick an empty square on the board"
          askForMove position
    _ -> do
      lift $ putStrLn "invalid input.  Please Make a move"
      askForMove position

play = do
  lift $ putStrLn "Welcome to Tic Tac Toe"
  maybePlayer <- askForPlayer
  case maybePlayer of
    Nothing -> return Quit
    Just player -> mainLoop player initPosition

mainLoop player position = do
  lift $ putStrLn (render position)
  if position `isWinFor` 'X' || position `isWinFor` 'O' then
    if player == Human
      then return ComputerWin
      else return HumanWin
  else if possibleMoves position == [] then return Draw
  else
    if player == Human
      then do
        maybeIdx <- askForMove position
        case maybeIdx of
          Nothing -> return Quit
          Just idx -> mainLoop Computer (move position idx)
      else do
        mainLoop Human (move position (evalState (bestMove position) M.empty))

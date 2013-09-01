module TTT.Position (
  Position (..)
 ,initPosition
 ,render
 ,move
 ,possibleMoves
 ,possiblePositions
 ,isWinFor
 ,spaces
 ,minimax
 ,bestMove
)  where

import Data.List
import Data.List.Split
import Data.Function

data Position = Position String Char deriving (Show, Eq)
dim = 3
size = dim^2

initPosition :: Position
initPosition = Position (replicate size ' ') 'X'

render :: Position -> String
render (Position board turn) = (
  unlines . intersperse (replicate (dim*4-1) '-') .
  map (concat . intersperse "|") . chunksOf dim .
  map (\(i,c) -> " "++(if c==' ' then show i else [c])++" ") .
  zip [0..]) board

other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'

move :: Position -> Int -> Position
move (Position board turn) idx = Position (update idx turn board) (other turn)
  where update 0 turn (_:xs) = turn:xs
        update idx turn (x:xs) = x:(update (idx - 1) turn xs)

possibleMoves :: Position -> [Int]
possibleMoves (Position board _) = (
  map fst . filter (\(i,c) -> c==' ') . zip [0..]) board

possiblePositions :: Position -> [Position]
possiblePositions position = map (position `move`) (possibleMoves position)

isWinFor :: Position -> Char -> Bool
isWinFor (Position board _) turn =
  any matchLine rows || any matchLine (transpose rows) ||
  matchLine (map (board !!) [0,dim+1..size-1]) ||
  matchLine (map (board !!) [dim-1,dim*2-2..size-2])
  where matchLine = all (==turn)
        rows = chunksOf dim board

spaces :: Position -> Int
spaces (Position board _) = length (filter (==' ') board)

choose :: Char -> a -> a -> a
choose 'X' a _ = a
choose 'O' _ b = b

minimax :: Position -> Int
minimax position@(Position _ turn)
  | position `isWinFor` 'X' = 100
  | position `isWinFor` 'O' = (-100)
  | spaces position == 0 = 0
  | otherwise =
    (choose turn add sub)
    ((choose turn maximum minimum (map minimax (possiblePositions position))))
    (spaces position)
  where add a b = a + b + 0
        sub a b = a - b + 0

-- bestMove :: Position -> Int
bestMove position = (
  map (snd ) .
  map (\idx -> (idx, minimax (position `move` idx))))
  (possibleMoves position)

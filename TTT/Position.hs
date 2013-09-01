module TTT.Position (
  Position (..)
 ,initPosition
 ,render
 ,move
 ,possibleMoves
 ,possiblePositions
 ,isWinFor
 ,minimax
 ,bestMove
)  where

import Data.List
import Data.List.Split

data Position = Position String Char deriving (Show, Eq)
dim = 3
size = 3^2

initPosition :: Position
initPosition = Position (replicate size ' ') 'X'

render :: Position -> String
render (Position board _) =
  (unlines .
  intersperse (replicate (dim*4-1) '-') .
  map (concat . intersperse "|") .
  chunksOf dim .
  map (\(i,c) -> " "++(if c==' ' then show i else [c])++" ") .
  zip [0..]) board

move :: Position -> Int -> Position
move (Position board turn) idx = Position (update idx board) (other turn)
  where update 0 (_:xs) = turn:xs
        update idx (x:xs) = x:(update (idx - 1) xs)
        other 'X' = 'O'
        other 'O' = 'X'

possibleMoves :: Position -> [Int]
possibleMoves (Position board _) =
  (map fst . filter ((==' ') . snd) . zip [0..]) board

possiblePositions :: Position -> [Position]
possiblePositions position = map (position `move`) (possibleMoves position)

isWinFor :: Position -> Char -> Bool
isWinFor (Position board _) turn =
  any lineMatch rows || any lineMatch (transpose rows) ||
  lineMatch (map (board !!) [0,dim+1..size-1]) ||
  lineMatch (map (board !!) [dim-1,dim*2-2..size-2])
  where rows = chunksOf dim board
        lineMatch = all (==turn)

spaces :: String -> Int
spaces = length . filter  (==' ')

choose :: Char -> a -> a -> a
choose 'X' x o = x
choose 'O' x o = o

minimax :: Position -> Int
minimax position@(Position board turn)
  | position `isWinFor` 'X' = 100
  | position `isWinFor` 'O' = (-100)
  | spaces board == 0 = 0
  | otherwise =
      ((choose turn maximum minimum) (map minimax (possiblePositions position)))
      (choose turn (+) (-))
      (spaces board)

-- bestMove :: Position -> Int
-- bestMove position@(Position board turn) =
--   (choose turn maxBy minBy)

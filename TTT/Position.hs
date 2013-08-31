module TTT.Position (
  Position (..)
 ,initPosition
 ,render
 ,move
 ,possibleMovesFor
 ,isWinFor
 ,evalLeaf
 ,minimax
)  where

import Data.List
import Data.List.Split

dim = 3
size = dim * dim

data Position = Position String Char deriving (Show, Eq)

initPosition :: Position
initPosition = Position (replicate size ' ') 'X'

render :: Position -> String
render (Position board turn) =
  (unlines .
  intersperse "-----------" .
   map (concat . intersperse "|") .
   chunksOf dim .
   zipWith (\idx turn -> " " ++ (case turn of
                                ' ' -> show idx
                                _ -> [turn]) ++ " ") [0..])
  board

other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'

update :: Int -> Char -> String -> String
update _ _ [] = []
update 0 turn (_:xs) =  turn:xs
update idx turn (x:xs) = x:(update (idx - 1) turn xs)

move :: Position -> Int -> Position
move (Position board turn) idx =
  Position (update idx turn board) (other turn)

possibleMovesFor :: Position -> [Int]
possibleMovesFor (Position board turn) =
  (map fst . filter (\(i,c) -> c == ' ') . zip [0..size-1]) board

isWinFor :: Position -> Char -> Bool
isWinFor (Position board _) turn =
  any isWinLine (chunksOf dim board) ||
  any isWinLine (transpose (chunksOf dim board)) ||
  isWinLine (map (board !!) [0,dim+1..size-1]) ||
  isWinLine (map (board !!) [dim-1,dim*2-2..size-2])
  where isWinLine line = all (==turn) line

countEmpty :: Position -> Int
countEmpty (Position board _) = (length . filter (==' ')) board

evalLeaf :: Position -> Maybe Int
evalLeaf position
  | position `isWinFor` 'X' = Just 100
  | position `isWinFor` 'O' = Just (-100)
  | countEmpty position == 0 = Just 0
  | otherwise = (Nothing :: Maybe Int)

minimax :: Position -> Int
minimax position = 100

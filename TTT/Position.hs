module TTT.Position (
  Position (..)
 ,initPosition
 ,render
 ,move
 ,possibleMoves
 ,isWinFor
 ,evalLeaf
 ,minimax
)  where

import Data.List
import Data.List.Split

data Position = Position String Char deriving (Show, Eq)
dim = 3
size = dim * dim

initPosition :: Position
initPosition = Position (replicate size ' ') 'X'

render :: Position -> String
render (Position board _) =
  (unlines . 
   intersperse (replicate (dim*4-1) '-') .
   map (concat . intersperse "|") .
   chunksOf dim .
   map (\(i,c) -> " " ++ (if c==' ' then show i else [c]) ++ " ") .
   zip [0..])
  board

update :: Int -> a -> [a] -> [a]
update 0 c (_:xs) = c:xs
update idx c (x:xs) = x:(update (idx - 1) c xs)

other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'

move :: Position -> Int -> Position
move (Position board turn) idx = Position (update idx turn board) (other turn)

possibleMoves :: Position -> [Int]
possibleMoves (Position board _) = listBlankIndexes board 0
  where listBlankIndexes [] _ = []
        listBlankIndexes (' ':xs) n = n:(listBlankIndexes xs (n + 1))
        listBlankIndexes (_:xs) n = listBlankIndexes xs (n + 1)

isWinFor :: Position -> Char -> Bool
isWinFor (Position board _) turn =
  any lineMatch rows || any lineMatch (transpose rows) ||
  lineMatch (map (board !!) [0,dim+1..size-1]) ||
  lineMatch (map (board !!) [dim-1,2*dim-2..size-2])
  where rows = chunksOf dim board
        lineMatch = all (==turn)

spaces :: String -> Int
spaces = length . filter (==' ')

evalLeaf :: Position -> Maybe Int
evalLeaf position@(Position board turn)
  | position `isWinFor` 'X' = Just 100
  | position `isWinFor` 'O' = Just (-100)
  | (spaces board) == 0 = Just 0
  | otherwise = Nothing

-- minimax :: Position -> Int
minimax position =
  case evalLeaf position of
    Just n -> n
    Nothing -> 99

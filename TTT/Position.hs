module TTT.Position (
  Position (..)
 ,new
 ,render
 ,move
 ,possibleMoves
 ,isWinFor
 ,evalLeaf
) where

import Data.List.Split
import Data.List

dim = 3
size = 9
data Position = Position String Char deriving (Show, Eq)

new :: Position
new = Position (replicate size ' ') 'X'

render :: Position -> String
render (Position board turn) =
  (unlines .
    intersperse "-----------" .
    map (\row -> ((concat . intersperse "|") row)) .
    chunksOf dim . map putCellContent . zip [0..]) board
  where putCellContent (idx, ' ') = " " ++ show idx ++ " "
        putCellContent (_, x) = [' ', x, ' ']

other :: Char -> Char
other 'X' = 'O'
other 'O' = 'X'

update :: Int -> Char -> String -> String
update 0 c (x:xs) = c:xs
update n c (x:xs) = x:(update (n - 1) c xs)

move :: Position -> Int -> Position
move (Position board turn) idx =
  Position (update idx turn board) (other turn)

possibleMoves (Position board turn) =
  (map fst . filter (\(x,y) -> y == ' ') . zip [0..]) board

isWinFor (Position board _) turn =
  any lineMatch (rows ++ (transpose rows)) ||
  lineMatch (map (\idx -> board !! idx) [0,(dim+1)..size]) ||
  lineMatch (map (\idx -> board !! idx) [(dim-1),(2*dim-2)..(size-2)])
  where lineMatch = all (\x -> x == turn)
        rows = chunksOf dim board

evalLeaf p = Nothing

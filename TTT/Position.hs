module TTT.Position (
  Position (..)
 ,dim
 ,size
 ,initPosition
 ,move
 ,choose
 ,possibleMoves
 ,isWinFor
)  where

import Data.List
import Data.List.Split

data Position = Position String Char deriving (Show, Eq, Ord)
dim = 3
size = dim^2

initPosition = Position (replicate size ' ') 'X'

move (Position board turn) idx = Position (update idx turn board) (other turn)
  where update 0 turn (_:xs) = turn:xs
        update idx turn (x:xs) = x:(update (idx - 1) turn xs)
        other turn = choose turn 'O' 'X'

choose 'X' x o = x
choose 'O' x o = o

possibleMoves (Position board turn) = ' ' `elemIndices` board

isWinFor (Position board _) turn =
  any isLineMatch rows || any isLineMatch (transpose rows) ||
  isLineMatch (map (board!!) [0,dim+1..size-1]) ||
  isLineMatch (map (board!!) [dim-1,dim*2-2..size-2])
  where isLineMatch = all (==turn)
        rows = chunksOf dim board

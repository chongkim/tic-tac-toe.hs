module TTT.Position (
  Position (..)
 ,initPosition
 ,render
 ,move
 ,possibleMoves
 ,isWinFor
 ,minimax
 ,bestMove
 ,isEnd
 ,isBlank
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
render (Position board turn) =
  unlines . intersperse (replicate (dim*4-1) '-') .
  map (concat . intersperse "|") . chunksOf dim .
  map (\(i,c) -> " "++(if c==' ' then show i else [c])++" ") .
  zip [0..] $ board

choose :: Char -> a -> a -> a
choose 'X' x _ = x
choose 'O' _ o = o

other :: Char -> Char
other turn = choose turn 'O' 'X'

move :: Position -> Int -> Position
move (Position board turn) idx =
  Position (update idx turn board) (other turn)
  where update 0 turn (_:xs) = turn:xs
        update idx turn (x:xs) = x:(update (idx - 1) turn xs)

possibleMoves :: Position -> [Int]
possibleMoves (Position board turn) =
  map fst . filter (\(i,c) -> c==' ') . zip [0..] $ board

isWinFor :: Position -> Char -> Bool
isWinFor (Position board _) turn =
  any matchLine rows || any matchLine (transpose rows) ||
  matchLine (map (board !!) [0,dim+1..size-1]) ||
  matchLine (map (board !!) [dim-1,dim*2-2..size-2])
  where rows = chunksOf dim board
        matchLine = all (==turn)

spaces :: Position -> Int
spaces (Position board turn) = length $ filter (==' ') board

minimax :: Position -> Int
minimax position@(Position board turn)
  | position `isWinFor` 'X' = 100
  | position `isWinFor` 'O' = (-100)
  | spaces position == 0 = 0
  | otherwise =
    (choose turn (+) (-))
    ((choose turn maximum minimum) . map (\i -> minimax $ position `move` i)
      $ possibleMoves position)
    (spaces position)

bestMove :: Position -> Int
bestMove position@(Position _ turn) =
  fst . (choose turn maximumBy minimumBy) (compare `on` snd) .
  map (\i -> (i, minimax $ position `move` i)) $ possibleMoves position

isEnd :: Position -> Bool
isEnd position =
  position `isWinFor` 'X' || position `isWinFor` 'O' || spaces position == 0

isBlank :: Position -> Bool
isBlank (Position board turn) = all (==' ') board

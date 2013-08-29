module Position where

import Data.List.Split
import Data.List.Utils

dim = 3
size = 9

data Position = Position [Char] Char
                deriving (Eq)
new :: Position
new = Position (replicate size ' ') 'X'

instance Show Position where
  show (Position board turn) = join "\n-----------\n"
                                    (map formatRow (chunksOf 3 board)) ++ "\n"
    where formatRow row = " " ++ (join " | " (chunksOf 1 row)) ++ " "

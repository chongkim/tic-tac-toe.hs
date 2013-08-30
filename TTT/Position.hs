module TTT.Position where

import Data.List.Split
import Data.List

size = 9
dim = 3

type Board = String
type Turn = Char

data Position = Position Board Turn

new = Position (replicate size ' ') 'X'
board (Position b _) = b
turn (Position _ t) = t

render (Position b _) = (unlines
                       . joinWithHorizontalLine
                       . addSpaces
                       . putBars
                       . breakIntoRows
                       . replaceSpaces
                       . pairUpWithIndex) b
  where pairUpWithIndex b = zip b [0..]
        putBars = map (\s -> intersperse '|' s)
        breakIntoRows = chunksOf dim
        addSpaces = map (\s -> " " ++ (intersperse ' ' s) ++ " ")
        replaceSpaces = map replaceSpace
        replaceSpace (' ',i) = show i !! 0
        replaceSpace (c,i) = c
        joinWithHorizontalLine = intersperse "-----------"

-- move (Position b t) idx = Position "X  "
--                                 ++ "   "
--                                 ++ "   " 'O'

move (Position b t) idx = Position ("X  "
                                 ++ "   "
                                 ++ "   ") 'a'

module TTT.Position (new
                    ,board
                    ,turn
                    ,render) where

import Data.List
import Data.List.Split

type Board = String
type Turn = Char

data Position = Position Board Turn

new = Position ("   "
            ++ "   "
            ++ "   ") 'X'

board (Position b _) = b
turn (Position _ t) = t

render = undefined

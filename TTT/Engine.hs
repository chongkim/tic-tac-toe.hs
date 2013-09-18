module TTT.Engine (
  minimax
 ,bestMove
)  where

import TTT.Position
import Control.Monad.State
import qualified Data.Map as M
import Data.List.Split
import Data.List
import Data.Function

minimax position@(Position board turn) = do
  cache <- get
  case M.lookup position cache of
    Just n -> return n
    Nothing -> storeMinimax position

storeMinimax position@(Position board turn)
  | position `isWinFor` 'X' = store 100
  | position `isWinFor` 'O' = store (-100)
  | moves == [] = store 0
  | otherwise = do
    list <- mapM (minimax . move position) moves
    store ((choose turn (+) (-)) ((choose turn maximum minimum) list)
            (length moves))
  where store n = do modify (M.insert position n); return n
        moves = possibleMoves position

bestMove position@(Position board turn) = do
  list <- mapM (minimax . move position) moves
  return (fst . (choose turn maximumBy minimumBy) (compare `on` snd) $ zip moves list)
  where moves = possibleMoves position

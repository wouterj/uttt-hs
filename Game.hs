module Game
( turn
) where

import Board
import Data.Tree

-- |plays a sequence of positions
playSeq :: [Pos] -> Board -> Board
playSeq seq brd = foldl play brd seq
    where play brd p = update p (turn brd) brd

-- |the player that has to make a move
turn :: Board -> Int
turn brd = player $ (count 1 board) - (count 2 board)
    where board = cells brd
          count p b = length $ filter (==p) b
          player 0 = 1
          player 1 = 2
          player x = error "invalid board state"

-- |generates a tree from the board
generate :: Board -> Tree Board
generate brd = unfoldTree (\v -> (v, map (nextState v) $ moves v)) brd
    where nextState b m = playSeq [m] b

-- |prunes tree at a certain depths
depthPrune :: Int -> Tree a -> Tree a
depthPrune 0 t = Node (rootLabel t) []
depthPrune d t = Node (rootLabel t) (map (depthPrune (d-1)) $ subForest t)

-- |game evaluation
-- | positive result = good for O
-- | negative result = good for X
staticVal :: Board -> Int
staticVal brd
    | winner == 1 = 1000
    | winner == 2 = -1000
    | otherwise   = 0
        where winner = boardWinner brd

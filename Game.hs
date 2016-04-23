module Game
( Game
, moves
, nextActiveSquares
, playSeq
, turn
, generate
, depthPrune
, staticVal
, gpretty'
) where

import Board
import Data.Tree

{--
 - Game
 --}
data Game = Game { board :: Board, activeSquares :: [Int] }
    deriving Show

initGame = Game emptyBoard (replicate 9 (-1))

-- |list of possible moves
moves :: Game -> [Pos]
moves game
    | boardWon brd || boardDrawn brd = []
    | otherwise                      = [fst c | c <- playableCells, snd c == 0]
        where playableCells = concat $ filter (\s -> 0 == winner (map snd s)) $ playableSquares
              playableSquares = map (\i -> square i brd) $ activeSquares game
              brd = board game

nextActiveSquares :: Pos -> Board -> [Int]
nextActiveSquares p brd
    | 0 /= winner (map (snd) (square index brd)) = freeIndices
    | otherwise                                  = [index]
        where index = miniIndex p
              freeIndices = filter (\s -> 0 == winner (map snd (square s brd))) [0..8]


-- |plays a sequence of positions
playSeq :: [Pos] -> Game -> Game
playSeq seq game = foldl play game seq
    where play game p = let brd = board game
                        in Game (update p (turn brd) brd) (nextActiveSquares p brd)

-- |the player that has to make a move
turn :: Board -> Int
turn brd = player $ (count 1 board) - (count 2 board)
    where board = cells brd
          count p b = length $ filter (==p) b
          player 0 = 1
          player 1 = 2
          player x = error "invalid board state"

{--
 - Minimax / Alpha-Beta prunning
 --}
-- |generates a tree from the game
generate :: Game -> Tree Game
generate game = unfoldTree (\v -> (v, map (nextState v) $ moves v)) game
    where nextState g m = playSeq [m] g

-- |prunes tree at a certain depths
depthPrune :: Int -> Tree a -> Tree a
depthPrune 0 t = Node (rootLabel t) []
depthPrune d t = Node (rootLabel t) (map (depthPrune (d-1)) $ subForest t)

-- |game evaluation
-- | positive result = good for O
-- | negative result = good for X
staticVal :: Game -> Int
staticVal (Game brd _)
    | winner == 1 = 1000
    | True `elem` map (wonByPlayer 1) [0,2,6,8] = 200
    | winner == 2 = -1000
    | True `elem` map (wonByPlayer 2) [0,2,6,8] = -200
    | otherwise   = 0
        where winner = boardWinner brd
              wonByPlayer sq = wonBy (map (snd) $ square sq brd)

{--
 - Debugging
 --}
gpretty' :: Game -> String
gpretty' = pretty' . board

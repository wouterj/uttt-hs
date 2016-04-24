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
    | 0 /= winner (map snd (square index brd)) = freeIndices
    | otherwise                                  = [index]
        where index = miniIndex p
              freeIndices = filter (\s -> 0 == winner (map snd (square s brd))) [0..8]


-- |plays a sequence of positions
playSeq :: [Pos] -> Game -> Game
playSeq seq game = foldl play game seq
    where play game p = let brd = board game
                            nextBoard = update p (turn brd) brd
                        in Game nextBoard (nextActiveSquares p nextBoard)

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
    | w == 1                                     = 1000
    | w == 2                                     = -1000
    | otherwise                                  = squareWinPoints + cornerWinPoints + almostWinPoints
    where w = boardWinner brd
          winners = map (\c -> winner (map snd c))
          squareWinners = winners $ squares brd
          cornerWinners = winners corners
          corners = map (\s -> square s brd) [0,2,6,8]
          almostSquareWins p = filter (\s -> nInARow s p 2) $ filter (\s -> 0 == winner s) $ map (\s -> map snd $ square s brd) [0..8]
          almostBoardWin p
            | nInARow (map winner $ map (map snd) $ squares brd) p 2 = 1
            | otherwise                                              = 0

          squareWinPoints = 50 * (length $ filter (==1) squareWinners) - 50 * (length $ filter (==2) squareWinners)
          cornerWinPoints
              | (length $ filter (==1) cornerWinners) >= 3 = 200
              | (length $ filter (==2) cornerWinners) >= 3 = -200
              | otherwise                                  = 0
          almostWinPoints = 40 * (length $ almostSquareWins 1) - 40 * (length $ almostSquareWins 2)
                            + 400 * (almostBoardWin 1) - 400 * (almostBoardWin 2)

maximizeTreeVal :: (Ord a) => Tree a -> [a]
maximizeTreeVal (Node x [])     = [x]
maximizeTreeVal (Node _ childs) = map maximum . map minimizeTreeVal $ childs

minimizeTreeVal :: (Ord a) => Tree a -> [a]
minimizeTreeVal (Node x [])     = [x]
minimizeTreeVal (Node _ childs) = map minimum . map maximizeTreeVal $ childs

{--
 - Debugging
 --}
gpretty' :: Game -> String
gpretty' = pretty' . board

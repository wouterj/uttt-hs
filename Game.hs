module Game
( Game(..)
, initGame
, turn
, nextActiveSquares
, play
, playSeq
, moves
, gpretty
, gpretty'
, gpprint
) where

import Board

{--
 - Game
 --}
data Game = Game { getBoard :: Board, getActiveSquares :: [Int] }
    deriving Show

initGame = Game emptyBoard [0..8]

-- |The player that has to make a turn
turn :: Board -> Int
turn board
    | t == 0    = 1
    | t == 1    = 2
    | otherwise = error "invalid board state"
    where t = (count 1 board) - (count 2 board)
          count p = length . filter (==p) . cells

-- |Make one move in the game
play :: Pos -> Int -> Game -> Game
play p i game = Game board activeSquares
    where board = update p i $ getBoard game
          activeSquares = nextActiveSquares p board

-- |A little play helper for folds
play' :: Game -> (Pos, Int) -> Game
play' game (p, i) = play p i game

-- |Play a sequence of moves
playSeq :: [Pos] -> Game -> Game
playSeq ps game = foldl play' game $ zip ps $ cycle [startPlayer, otherPlayer startPlayer]
    where startPlayer = turn $ getBoard game
          otherPlayer 1 = 2
          otherPlayer 2 = 1

-- |The available squares when making the move
nextActiveSquares :: Pos -> Board -> [Int]
nextActiveSquares p board
    | 0 /= winner (map snd (square index board)) = freeIndices
    | otherwise                                  = [index]
        where index = miniIndex p
              freeIndices = filter (\s -> 0 == winner (map snd (square s board))) [0..8]

-- |Possible moves by the current player
moves :: Game -> [(Pos, Game)]
moves game
    | boardFinished board = []
    | otherwise           = [(fst c, play (fst c) (turn board) game) | c <- concat playableSquares, snd c == 0]
    where board = getBoard game
          playableSquares = map (\i -> square i board) $ getActiveSquares game

{--
 - Debug
 --}
gpretty :: Game -> String
gpretty = pretty . getBoard

gpretty' :: Game -> String
gpretty' = pretty' . getBoard

gpprint :: Game -> IO ()
gpprint = pprint . getBoard

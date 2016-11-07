module Heuristic
( evaluate
) where

import Board(boardWinner, winner, square, squares, nInARow', nInARow, boardFinished)
import Game(Game(..), turn)
import Debug.Trace(trace)

-- |Static evaluation
-- | Higher values are good for player 1, lower voor 2
evaluate :: Game -> Int
evaluate game
    | boardWin game /= 0 = boardWin game
    | otherwise          = foldl (\score h -> score + (h game)) 0 [squareWins, almostSquareWins, almostBoardWins, activeSquares, cellPositions]

boardWin game
    | winner == 1 = 50
    | winner == 2 = -50
    | otherwise   = 0
    where winner = boardWinner $ getBoard game

squareWins game = sum s
    where s = map (\(i, s) -> norm i $ winner $ map snd s) $ zip [0..] $ squares $ getBoard game
          norm _ 0 = 0
          norm i 1
            | i `elem` [0,2,6,8] = 3
            | i == 4             = 5
            | otherwise          = 2
          norm i 2 = negate $ norm i 1

almostSquareWins game = (1 * twoCells 1 game) - (1 * twoCells 2 game)
    where twoCells p = length . filter (\s -> nInARow (map snd s) p 2) . filter ((0==) . winner . map snd) . squares . getBoard

almostBoardWins game = 10 * (forks 1) - 10 * (forks 2)
    where board = map (winner . map snd) $ squares $ getBoard game
          forks p = nInARow' board p 2

activeSquares game
    | (length squares) > 1    = score * 10
    | almostSquareWin squares = score * 7
    | otherwise               = 0
    where squares = getActiveSquares game
          board = getBoard game
          nextTurn = turn board
          almostSquareWin [] = False
          almostSquareWin ss = nInARow (map snd $ square (head ss) board) nextTurn 2
          score = if 1 == nextTurn
                    then 1
                    else -1

cellPositions game = sum $ map (rateCells) $ squares board
    where board = getBoard game
          rateCells cells
            | center == 1 = 1
            | center == 2 = -1
            | otherwise   = 0
            where center = snd $ cells !! 4

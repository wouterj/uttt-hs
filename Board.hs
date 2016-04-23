module Board
( Pos, Cell, Board
, emptyBoard
, cells
, square
, squares
, moves
, update
, boardWinner
, boardWon
, boardDrawn
, winner
, pretty
, pprint
, pretty'
) where

import Data.Array.IArray
import Util.List
import Data.List(transpose, intercalate)

type Pos = (Int, Int)
type Cell = Int
type Board = Array Pos Cell

emptyBoard :: Board
emptyBoard = listArray ((0, 0), (8, 8)) $ replicate 81 0

-- |all cells of a board
cells :: Board -> [Cell]
cells = elems

-- |get a square by index
square :: Int -> Board -> [(Pos, Cell)]
square i brd = [n | n <- assocs brd, (y n) `elem` ys && (x n) `elem` xs]
    where x = fst . fst
          y = snd . fst
          minX = (mod i 3) * 3
          xs = take 3 [minX..]
          minY = (quot i 3) * 3
          ys = take 3 [minY..]

-- |get all squares
squares :: Board -> [[(Pos, Cell)]]
squares brd = map (\i -> square i brd) [0..8]

-- |list of possible moves
moves :: Board -> [Pos]
moves brd
    | boardWon brd || boardDrawn brd = []
    | otherwise                      = [fst c | c <- playableCells, snd c == 0]
        where playableCells = concat $ filter (\s -> 0 == winner (map snd s)) $ playableSquares
              playableSquares = squares brd

-- |updates one cell
update :: Pos -> Int -> Board -> Board
update p v brd = brd // [(p, v)]

-- |the winner of the board (or 0)
boardWinner :: Board -> Int
boardWinner = winner . map (winner) . map (map snd) . squares

-- |checks if the board has a winner
boardWon :: Board -> Bool
boardWon = (/=0) . boardWinner

-- |checks if the board is drawn (no winner & no moves left)
boardDrawn :: Board -> Bool
boardDrawn brd = 0 == freeCells && not (boardWon brd)
    where freeCells = length $ filter (==0) $ cells brd

-- |generic winner checker, can be used for squares
winner :: (Num a, Eq a) => [a] -> a
winner xs = player $ filter (full) $ diagonals ys ++ ys ++ columns ys
    where
        ys = chunks 3 $ xs
        full [a, b, c] = a == b && b == c && a /= 0
        columns = transpose
        diagonals [[a, _, b], [_, c, _], [d, _, e]] = [[a, c, e], [d, c, b]]
        player xs
            | 0 == length xs = 0
            | otherwise      = head $ head xs

{--
 - Debugging helpers
 --}
pretty :: Board -> String
pretty = intercalate divider . map (intercalate "\n") . chunks 3 . rows
    where rows = map row . chunks 9 . map cell . cells
          row = intercalate " | " . chunks 3
          divider = "\n----+-----+----\n"
          cell 0 = '.'
          cell 1 = 'O'
          cell 2 = 'X'

-- |pretty print the board
pprint :: Board -> IO ()
pprint = putStrLn . pretty

-- |a one line representation of the board
pretty' :: Board -> String
pretty' = map cell . cells
    where cell 0 = '.'
          cell 1 = 'O'
          cell 2 = 'X'

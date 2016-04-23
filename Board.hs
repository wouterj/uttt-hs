module Board
( Pos, Cell, Board
, emptyBoard
, cells
, square
, squares
, miniIndex
, update
, boardWinner
, boardWon
, boardDrawn
, winner
, wonBy
, pretty
, pprint
, pretty'
) where

import Data.Array.IArray
import Util.List
import Data.List(transpose, intercalate)
import Data.Char(intToDigit)

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

-- |get the index of the cell in the square
miniIndex :: Pos -> Int
miniIndex (x, y) = (mod y 3) * 3 + mod x 3

-- |updates one cell
update :: Pos -> Int -> Board -> Board
update (x, y) v brd
    | x > 8 || x < 0 = error ("x out of range: " ++ [intToDigit x])
    | y > 8 || y < 0 = error ("y out of range: " ++ [intToDigit y])
    | otherwise = brd // [((x, y), v)]

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

wonBy :: (Num a, Eq a) => [a] -> a -> Bool
wonBy xs p = p == winner xs

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
    where rows = map row . transpose . chunks 9 . map cell . cells
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
pretty' = concat . transpose . chunks 9 . map cell . cells
    where cell 0 = '.'
          cell 1 = 'O'
          cell 2 = 'X'

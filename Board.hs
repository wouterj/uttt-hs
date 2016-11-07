module Board
( Pos, Cell, Board
, emptyBoard
, boardFromString
, boardFromString'
, cells
, square
, squares
, miniIndex
, update
, update'
, boardWinner
, boardWon
, boardDrawn
, boardFinished
, nInARow'
, nInARow
, winner
, wonBy
, pretty
, pprint
, pretty'
) where

import Data.List(transpose, intercalate)
import Data.Char(intToDigit, digitToInt)

type Pos = (Int, Int)
type Cell = Int
type Square = [(Pos, Cell)]
type Board = [(Pos, Cell)]

emptyBoard :: Board
emptyBoard = [((x, y), 0) | x <- [0..8], y <- [0..8]]

boardFromString :: String -> Board
boardFromString = boardFromString' digitToInt

boardFromString' :: (Char -> Int) -> String -> Board
boardFromString' f board = zip positions $ reverse $ foldl toCell [] board
    where positions = [(x, y) | y <- [0..8], x <- [0..8]]
          toCell xs s
            | s == ','  = xs
            | otherwise = (f s) : xs

-- |all cells of a board
cells :: Board -> [Cell]
cells = map snd

-- |get a square by index
square :: Int -> Board -> Square
square i board = [n | n <- board, (snd $ fst n) `elem` ys && (fst $ fst n) `elem` xs]
    where minX = (mod i 3) * 3
          xs = take 3 [minX..]
          minY = (quot i 3) * 3
          ys = take 3 [minY..]

-- |get all squares
squares :: Board -> [Square]
squares brd = map (\i -> square i brd) [0..8]

-- |get the index of the cell in the square
miniIndex :: Pos -> Int
miniIndex (x, y) = (mod y 3) * 3 + mod x 3

-- |updates one cell
update :: Pos -> Int -> Board -> Board
update pos value = foldl update' []
    where update' xs x
            | (fst x) == pos = (pos, value) : xs
            | otherwise      = x : xs

-- |updates one cell, created for easy fold usage
update' :: Board -> (Pos, Int) -> Board
update' board (pos, value) = update pos value board

-- |the winner of the board (or 0)
boardWinner :: Board -> Int
boardWinner = winner . map (winner) . map (map snd) . squares

-- |checks if the board has a winner
boardWon :: Board -> Bool
boardWon = (/= 0) . boardWinner

-- |checks if the board is drawn (no winner & no moves left)
boardDrawn :: Board -> Bool
boardDrawn brd = 0 == freeCells && not (boardWon brd)
    where freeCells = length $ filter (==0) $ cells brd

-- |checks if the board is either won or drawn
boardFinished :: Board -> Bool
boardFinished board = boardWon board || boardDrawn board

nInARow' :: (Num a, Eq a) => [a] -> a -> Int -> Int
nInARow' xs p i = length $ filter (\s -> i == sum s) $ diagonals ys ++ ys ++ columns ys
    where ys = chunks 3 $ map norm xs
          norm c
              | c == p    = 1
              | c == 0    = 0
              | otherwise = -3
          columns = transpose
          diagonals [[a, _, b], [_, c, _], [d, _, e]] = [[a, c, e], [d, c, b]]

-- |checks if there are i numbers in a row with possibility to create a match
nInARow :: (Num a, Eq a, Ord a) => [a] -> a -> Int -> Bool
nInARow xs p i = 0 < (nInARow' xs p i)

-- |checks if the specified player won
wonBy :: (Num a, Eq a) => [a] -> a -> Bool
wonBy xs p = p == winner xs

-- |generic winner checker, can be used for squares
winner :: (Num a, Eq a) => [a] -> a
winner xs = player $ filter (full) $ diagonals ys ++ ys ++ columns ys
    where
        ys = chunks 3 xs
        full [a, b, c] = a == b && b == c && a /= 0
        columns = transpose
        diagonals [[a, _, b], [_, c, _], [d, _, e]] = [[a, c, e], [d, c, b]]
        player cells
            | 0 == length cells = 0
            | otherwise         = head $ head cells

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

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : (chunks n $ drop n xs)

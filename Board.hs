module Board
( Pos, Square, SinglePlayerBoard, Board
, emptyBoard
, singlePlayerBoard
, bitFromPos
, squareIndex
) where

import Data.Word(Word16(..))
import Data.Bits((.&.), (.|.), xor, shiftL, shiftR)
import Data.List(intercalate, transpose)
import Data.Char(intToDigit, digitToInt)
import Data.Ord(comparing)

type Pos = (Int, Int)
type Cell = Int
type Square = Word16
type SinglePlayerBoard = (Square, Square, Square, Square, Square, Square, Square, Square, Square)
type Board = (SinglePlayerBoard, SinglePlayerBoard)

emptyBoard :: Board
emptyBoard = ((0,0,0,0,0,0,0,0,0), (0,0,0,0,0,0,0,0,0))

singlePlayerBoardToList :: SinglePlayerBoard -> [Square]
singlePlayerBoardToList (s0, s1, s2, s3, s4, s5, s6, s7, s8) = [s0, s1, s2, s3, s4, s5, s6, s7, s8]

singlePlayerBoard :: Int -> Board -> SinglePlayerBoard
singlePlayerBoard player board
    | player == 1 = fst board
    | otherwise   = snd board

bitFromPos :: Pos -> Int
bitFromPos (x, y) = ((2 - y) `mod` 3) + 4 * (x `mod` 3)

boardFromString :: String -> Board
boardFromString = boardFromString' digitToInt

boardFromString' :: (Char -> Int) -> String -> Board
boardFromString' f board = foldl toBoard 0 boardList
    where index = shiftL 1
          boardList = zip [1..] $ foldr toCell [] board
          toBoard = index 1
          toCell s xs
            | s == ','  = xs
            | otherwise = (f s) : xs

{--
-- |all cells of a board
cells :: Board -> [Cell]
cells = M.elems

-- |get a square by index
square :: Int -> Board -> Square
square i board = map (\p -> (p, board M.! p)) pos
    where minX = (mod i 3) * 3
          xs = take 3 [minX..]
          minY = (quot i 3) * 3
          ys = take 3 [minY..]
          pos = [(x, y) | y <- ys, x <- xs]

-- |get all squares
squares :: Board -> [Square]
squares brd = map (\i -> square i brd) [0..8]

-- |get the index of the cell in the square
miniIndex :: Pos -> Int
miniIndex (x, y) = (mod y 3) * 3 + mod x 3
--}

-- |get the square index of the cell
squareIndex :: Pos -> Int
squareIndex (x, y) = 3 * (y `div` 3) + (x `div` 3)

opponent :: Int -> Int
opponent 1 = 2
opponent 2 = 1

updateBoard :: Int -> Int -> (Word16 -> Word16) -> Board -> Board
updateBoard player squareIndex f board
    | player == 1 = (updateSinglePlayer $ singlePlayerBoard 1 board, snd board)
    | player == 2 = (fst board, updateSinglePlayer $ singlePlayerBoard 2 board)
    where updateSinglePlayer (s0, s1, s2, s3, s4, s5, s6, s7, s8)
            | squareIndex == 0 = (f s0, s1, s2, s3, s4, s5, s6, s7, s8)
            | squareIndex == 1 = (s0, f s1, s2, s3, s4, s5, s6, s7, s8)
            | squareIndex == 2 = (s0, s1, f s2, s3, s4, s5, s6, s7, s8)
            | squareIndex == 3 = (s0, s1, s2, f s3, s4, s5, s6, s7, s8)
            | squareIndex == 4 = (s0, s1, s2, s3, f s4, s5, s6, s7, s8)
            | squareIndex == 5 = (s0, s1, s2, s3, s4, f s5, s6, s7, s8)
            | squareIndex == 6 = (s0, s1, s2, s3, s4, s5, f s6, s7, s8)
            | squareIndex == 7 = (s0, s1, s2, s3, s4, s5, s6, f s7, s8)
            | squareIndex == 8 = (s0, s1, s2, s3, s4, s5, s6, s7, f s8)

singlePlayerSquare :: Int -> SinglePlayerBoard -> Square
singlePlayerSquare index (s0, s1, s2, s3, s4, s5, s6, s7, s8)
    | index == 0 = s0
    | index == 1 = s1
    | index == 2 = s2
    | index == 3 = s3
    | index == 4 = s4
    | index == 5 = s5
    | index == 6 = s6
    | index == 7 = s7
    | index == 8 = s8

-- |updates one cell
update :: Pos -> Int -> Board -> Board
update pos player board = updateBoard player (squareIndex pos) makeMove board
    where makeMove = xor (1 `shiftL` (bitFromPos pos))

-- |updates one cell, created for easy fold usage
update' :: Board -> (Pos, Int) -> Board
update' board (pos, value) = update pos value board

squareWinner :: Int -> Board -> Int
squareWinner squareIndex board
    | playerWon 1 = 1
    | playerWon 2 = 2
    | otherwise   = 0
    where playerWon player = hasWinner $ singlePlayerSquare squareIndex $ singlePlayerBoard player board

boardWinner :: Board -> Int
boardWinner board
    | playerWon 1 = 1
    | playerWon 2 = 2
    | otherwise   = 0
    where squareWins = map (\i -> squareWinner i board) [0..8]
          playerWon player = hasWinner $ listToWord16 $ filter (\(_, v) -> v == player) $ zip [0..] squareWins
          listToWord16 [] = 0
          listToWord16 ((shifts, _):xs) = (1 `shiftL` shifts) .|. (listToWord16 xs)

hasWinner :: Word16 -> Bool
hasWinner square = True `elem` (map (shiftCompare square) [[1, 1], [4, 4], [3, 3], [5, 5]])
    where shiftCompare square shifts = 0 /= (foldl (\square i -> square .&. (square `shiftR` i)) square $ 0:shifts)

-- |checks if the board has a winner
boardWon :: Board -> Bool
boardWon = (/= 0) . boardWinner

boardDrawn :: Board -> Bool
boardDrawn board = (not $ boardWon board) && (0 == (length $ freeCells))
    where freeSquares = filter (\i -> 0 == (squareWinner i board)) [0..8]
          freeCells = filter (\i -> 1911 /= ((squareForPlayer 1 i) .|. (squareForPlayer 2 i))) freeSquares
          squareForPlayer player i = singlePlayerSquare i $ singlePlayerBoard player board

-- |checks if the board is either won or drawn
boardFinished :: Board -> Bool
boardFinished board = boardWon board || boardDrawn board

{--
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
--}

{--
 - Debugging helpers
 --}
pretty :: Board -> String
pretty = intercalate divider . map (intercalate "\n") . chunks 3 . rows
    where rows = map row . transpose . chunks 9 . map cell . cells
          row = intercalate " | " . chunks 3
          divider = "\n----+-----+----\n"
          cell (-1) = '_'
          cell 0    = '.'
          cell 1    = 'O'
          cell 2    = 'X'

-- |pretty print the board
pprint :: Board -> IO ()
pprint = putStrLn . pretty

-- |a one line representation of the board
pretty' :: Board -> String
pretty' board = concat $ map (toStr) $ singlePlayerBoardToList $ singlePlayerBoard 1 board
    where toStr square = foldr (cellToStr square) [] [0..8]
          cellToStr b i s
            | (b .&. (1 `shiftL` i)) == 0 = '.':s
            | otherwise                   = 'O':s

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : (chunks n $ drop n xs)

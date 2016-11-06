module AlphaBeta
( Evaluation
, score
, move
, game
, generateTree
, generateTree'
, preSort
, inherit
, inherit'
, depthPrune
, applyEvaluation
, bestMoves
, etprint
) where

import Data.Tree(Tree(..), unfoldTree, rootLabel, drawTree)
import Data.List(sortBy, maximumBy, minimumBy)
import Data.Ord(comparing)

import Debug.Trace(trace)

import Game
import Board
import Heuristic as H

{--
 - Evaluation
 --}
type Evaluation = (Int, Pos, Game)

score :: Evaluation -> Int
score (s, _, _) = s

move :: Evaluation -> Pos
move (_, m, _) = m

game :: Evaluation -> Game
game (_, _, g) = g

updateScore :: (Int -> Int) -> Evaluation -> Evaluation
updateScore f (s, p, g) = (f s, p, g)

{--
 - Game trees
 --}

-- |Generates a game tree
generateTree :: Game -> Tree (Pos, Game)
generateTree = generateTree' (\_ _ -> EQ)

-- |Generates a game tree
-- | The moves are pre-sorted by the f function to allow
-- | better alpha-beta pruning.
generateTree' :: ((Pos, Game) -> (Pos, Game) -> Ordering) -> Game -> Tree (Pos, Game)
generateTree' f game = unfoldTree (\(p, game) -> ((p, game), sortBy f $ moves game)) ((-1, -1), game)

-- |Applies the heuristic evaluation to the game tree
applyEvaluation :: Tree (Pos, Game) -> Tree Evaluation
applyEvaluation = fmap (\(p, g) -> (H.evaluate g, p, g))

-- |Prunes the tree to certain depths
depthPrune :: Int -> Tree a -> Tree a
depthPrune 0 t = Node (rootLabel t) []
depthPrune d t = Node (rootLabel t) (map (depthPrune (d-1)) $ subForest t)

-- |Makes one move in the tree
inherit :: Pos -> Tree Evaluation -> Tree Evaluation
inherit mv tree = search mv $ subForest tree
    where search :: Pos -> [Tree Evaluation] -> Tree Evaluation
          search m [] = error "Illegal move"
          search m (t:ts)
              | m == (move $ rootLabel t) = updateRootNode t
              | otherwise                 = search m ts

inherit' :: Board -> Tree Evaluation -> Tree Evaluation
inherit' board tree = search board $ subForest tree
    where search b [] = error "Illegal board"
          search b (t:ts)
              | b == (getBoard $ game $ rootLabel t) = updateRootNode t
              | otherwise                            = search b ts

updateRootNode :: Tree Evaluation -> Tree Evaluation
updateRootNode (Node (s, _, g) leafes) = Node (s, (-1,-1), g) leafes

{--
 - Negamax
 --}
preSort :: (Pos, Game) -> (Pos, Game) -> Ordering
preSort (_, game1) (_, game2)
    | (boardWinner $ getBoard game1) /= 0 = LT
    | (boardWinner $ getBoard game2) /= 0 = GT
    | otherwise = compare choices1 choices2
    where choices1 = length $ getActiveSquares game1
          choices2 = length $ getActiveSquares game2

mapmax :: (Int, Int) -> Evaluation -> [Evaluation] -> Evaluation
mapmax _ best [v] = maximumBy (comparing score) [best, v]
mapmax (a, b) best (v:vs)
    | a' >= b                           = v
    | boardFinished $ getBoard $ game v = v
    | otherwise                         = mapmax (a', b) best' vs
    where best' = maximumBy (comparing score) [best, v]
          a' = maximum [a, score v]

negamax :: Int -> (Int, Int) -> Tree Evaluation -> Evaluation
negamax color _ (Node eval [])        = updateScore (*color) eval
negamax color (a, b) (Node eval subs) = updateEval eval best
    where mapmax' = mapmax (a, b) ((-100, undefined, undefined))
--    where mapmax' = maximumBy (comparing score)
          best = updateScore negate $ mapmax' $ map (negamax (-color) ((-b), (-a))) subs
          updateEval (_, (-1,-1), _) b = b
          updateEval (_, m, g) b       = (score b, m, g)

negamax' :: Tree Evaluation -> Evaluation
negamax' = updateScore negate . negamax (-1) (-100, 100)

{--
 - Move search
 --}
-- |The best moves calculated
-- | Searches through deeper levels with a dummy result as head
bestMoves :: Int -> Tree Evaluation -> [Evaluation]
bestMoves player tree = dummyMove : map (\i -> (nega player) $ depthPrune i tree) (1:[2, 4..])
    where nega 1 = negamax'
          nega 2 = negamax 1 (-20, 20)
          dummyMove = rootLabel $ head $ subForest tree

{--
 - Debug
 --}
tprint :: Tree (Pos, Game) -> IO ()
tprint = putStrLn . drawTree . fmap (\(p, game) -> show p ++ " " ++ gpretty' game)

etprint :: Tree Evaluation -> IO ()
etprint = putStrLn . drawTree . fmap(\(s, p, g) -> show s ++ " for " ++ show p ++ gpretty' g)

epretty :: Evaluation -> String
epretty (score, move, game) = "S: " ++ show score ++ " on " ++ show move ++ "\nBoard:\n" ++ gpretty game

eprint :: Evaluation -> IO ()
eprint = putStrLn . epretty

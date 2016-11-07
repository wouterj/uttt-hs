{-# LANGUAGE BangPatterns #-}
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
, negamax
, bestMoves
, dummyMove
, etprint
, epretty
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
preSort (pos1, game1) (pos2, game2)
    | (boardWinner $ getBoard game1) /= 0  = LT
    | (boardWinner $ getBoard game2) /= 0  = GT
    | isCorner pos1 && (not $ isCorner pos2) = LT
    | isCorner pos2 && (not $ isCorner pos1) = GT
    | otherwise = compare choices1 choices2
    where choices1 = length $ getActiveSquares game1
          choices2 = length $ getActiveSquares game2
          isCorner p = p `elem` [(0,0),(0,2),(0,3),(0,5),(0,6),(0,8),(2,0),(2,2),(2,3),(2,5),(2,6),(2,8),(3,0),(3,2),(3,3),(3,5),(3,6),(3,8),(5,0),(5,2),(5,3),(5,5),(5,6),(5,8),(6,0),(6,2),(6,3),(6,5),(6,6),(6,8),(8,0),(8,2),(8,3),(8,5),(8,6),(8,8),(1,1),(1,4),(1,7),(4,1),(4,4),(4,7),(7,1),(7,4),(7,7)]

mapmax :: Int -> Int -> (Int, [Pos]) -> [(Int, [Pos])] -> (Int, [Pos])
mapmax _ _ best [v]    = maximumBy (comparing fst) [best, v]
mapmax a b best (v:vs)
    | a' >= b                           = v
    | otherwise                         = mapmax a' b best' vs
    where best' = maximumBy (comparing fst) [best, v]
          a' = maximum [a, fst v]

negamax :: Int -> (Int, Int) -> Tree Evaluation -> (Int, [Pos])
negamax color _ (Node (score, move, _) []) = (color * score, [move])
negamax color (a, b) (Node (_, move, _) subs)   = (pvv, move:pvm)
    where (pvv, pvm) = negaLevel (-1000, []) a b subs
          negaLevel prev_best@(score1, _) prev_a b (x:xs)
              | score1 < b = negaLevel best4 a b xs
              where best4 = case neg $ negamax (-color) ((-b), (-a')) x of
                                value@(score2, _) | (score2 > score1) -> value
                                                  | otherwise         -> prev_best
                    a' = maximum [score1, prev_a]
          negaLevel best _ _ _ = best
          neg (score, ms) = (-score, ms)

negamax' :: Int -> Tree Evaluation -> (Int, [Pos])
negamax' player = negamax color ((-1000), 1000)
    where color = if player == 1
                    then 1
                    else (-1)

{--
 - Move search
 --}
-- |The best moves calculated
-- | Searches through deeper levels with a dummy result as head
bestMoves :: Int -> Tree Evaluation -> [(Int, [Pos])]
bestMoves player tree = map (\i -> removeDummyRoot $ negamax' player $ depthPrune i tree) (1:[2, 4..])
    where removeDummyRoot (!s, moves) = (s, tail moves)

dummyMove tree = (score dummy, [move dummy])
    where dummy = rootLabel $ head $ subForest tree

{--
 - Debug
 --}
tprint :: Tree (Pos, Game) -> IO ()
tprint = putStrLn . drawTree . fmap (\(p, game) -> show p ++ " " ++ gpretty' game)

etprint :: Tree Evaluation -> IO ()
etprint = putStrLn . drawTree . fmap(\(s, p, g) -> show s ++ " for " ++ show p ++ gpretty' g)

etpretty :: Tree Evaluation -> String
etpretty = drawTree . fmap(\(s, p, g) -> show s ++ " for " ++ show p ++ gpretty' g)

epretty :: Evaluation -> String
epretty (score, move, game) = "S: " ++ show score ++ " on " ++ show move-- ++ ": " ++ gpretty' game

eprint :: Evaluation -> IO ()
eprint = putStrLn . epretty

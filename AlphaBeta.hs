{-# LANGUAGE BangPatterns #-}
module AlphaBeta
( Evaluation
, score
, move
, game
, generateTree
, generateTree'
, inherit
, inherit'
, depthPrune
, applyEvaluation
, negamax
, bestMoves
, dummyMove
, tprint
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
inherit' board tree = takeFirst $ dropWhile (not . search) $ subForest tree
    where search t = board `isEqual` (getBoard $ game $ rootLabel t)
          takeFirst []    = error "Illegal board"
          takeFirst (x:_) = x

updateRootNode :: Tree Evaluation -> Tree Evaluation
updateRootNode (Node (s, _, g) leafes) = Node (s, (-1,-1), g) leafes

{--
 - Negamax
 --}
mapmax :: Int -> Int -> (Int, [Pos]) -> [(Int, [Pos])] -> (Int, [Pos])
mapmax _ _ best [v]    = maximumBy (comparing fst) [best, v]
mapmax a b best (v:vs)
    | a' >= b                           = v
    | otherwise                         = mapmax a' b best' vs
    where best' = maximumBy (comparing fst) [best, v]
          a' = maximum [a, fst v]

negamax :: Int -> Int -> Int -> Tree Evaluation -> (Int, [Pos])
negamax color _ _ (Node (score, move, _) []) = (color * score, [move])
negamax color a b (Node (_, move, _) subs)   = (pvv, move:pvm)
    where (pvv, pvm) = negaLevel (-1000, []) a b subs
          negaLevel best@(score1, _) a b (x:xs)
              | score1 < b = negaLevel best' a b xs
              where best' = case neg $ negamax (-color) (-b) (-a') x of
                                value@(score2, _) | (score2 > score1) -> value
                                                  | otherwise         -> best
                    a' = maximum [score1, a]
          negaLevel best _ _ _ = best
          neg (score, ms) = (-score, ms)

negamax' :: Int -> Tree Evaluation -> (Int, [Pos])
negamax' player = negamax color (-1000) 1000
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

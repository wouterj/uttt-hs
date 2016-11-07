import System.IO(hPutStrLn, hFlush, stderr, stdout, isEOF)
import System.Time(ClockTime, TimeDiff(TimeDiff), tdSec, tdPicosec, diffClockTimes, getClockTime, addToClockTime)
import System.Timeout(timeout)
import Control.Monad.Trans.State
import Control.Monad.IO.Class(liftIO)
import Control.Monad(unless)
import Data.Tree(Tree, rootLabel)

import Board(Pos, emptyBoard, boardFromString, boardFinished, pretty)
import Game(Game(..), playSeq, initGame, getBoard, gpretty')
import AlphaBeta(Evaluation, applyEvaluation, score, move, bestMoves, dummyMove, generateTree, game, inherit', inherit, epretty)

data GameState = GameState {
    botId :: Int,
    moveNr :: Int,
    tree :: Tree Evaluation
} deriving Show

type Context = StateT GameState IO

timeoutList :: ClockTime -> (Int, [Pos]) -> [(Int, [Pos])] -> Int -> Context ([Pos], Int, Int)
timeoutList timeLimit (prev_score, prev_moves) es depth = do
    time <- liftIO getClockTime
    ex <- liftIO $ if time < timeLimit
        then do
            timeout (toMicrosec (diffClockTimes timeLimit time)) (return $! head es)
        else return Nothing

    case ex of
        Nothing -> return (prev_moves, prev_score, depth)
        Just e' -> do
            if (depth > 81)
                then return (prev_moves, prev_score, depth)
                else timeoutList timeLimit e' (tail es) (succ depth)

    where toMicrosec timeDiff = (tdSec timeDiff) * 1000000 + fromInteger (tdPicosec timeDiff `div` 1000000)

{--
 - Commands
 --}
settings :: [String] -> Context ()
settings ["your_botid", i] = do
    gamestate <- get
    put $ gamestate{ botId = read i }

settings _ = return ()

update :: [String] -> Context ()
update ["game", "move", nr] = do
    state <- get
    put $ state{ moveNr = read nr }

    return ()

update ["debug", "field", field, activeStr] = do
    state <- get
    let board = boardFromString field
        activeSquares = splitInts activeStr

    put $ state{ tree = applyEvaluation $ generateTree $ Game board activeSquares }
    where splitInts :: String -> [Int]
          splitInts [] = []
          splitInts [x] = [read $ x:[]]
          splitInts (x:_:xs) = (read $ x:[]) : splitInts xs

update ["game", "field", field] = do
    state <- get
    let board = boardFromString field

    if (moveNr state) == 1
        then return ()
        else put $ state{ tree = inherit' board $ tree state }

update _ = return ()

action :: [String] -> Context ()
action ["move", timebank] = do
    state <- get
    time <- liftIO getClockTime
    let normalDelay = if (read timebank) >= 1000
                        then TimeDiff 0 0 0 0 0 0 850000000000
                        else TimeDiff 0 0 0 0 0 0 500000000000

    liftIO $ hPutStrLn stderr $ "Current board: " ++ (gpretty' $ game $ rootLabel $ tree state)

    (moves, score, depth) <- timeoutList (addToClockTime normalDelay time) (dummyMove $ tree state) (bestMoves (botId state) $ tree state) 0
    let (x, y) = head moves
    
    liftIO $ hPutStrLn stderr $ "Score: " ++ show score ++ "; Depth: " ++ show depth ++ "; Moves: " ++ show moves
    liftIO $ putStrLn $ "place_move " ++ show x ++ " " ++ show y

    put $ state{ tree = inherit (x, y) $ tree state }

action _ = return ()

{--
 - Front controller
 --}
dispatch = [("settings", settings),
            ("update", update),
            ("action", action)]

loop = do
    command <- liftIO getLine

    --liftIO $ hPutStrLn stderr $ ">> " ++ command

    let (cmd:args) = words command

    case cmd of
        "settings" -> settings args
        "update"   -> update args
        "action"   -> action args

    liftIO $ hFlush stdout
    liftIO $ hFlush stderr

    eof <- liftIO isEOF
    unless eof loop

main = do
    evalStateT (loop) $ GameState{ botId = 0, moveNr = 0, tree = applyEvaluation $ generateTree initGame }

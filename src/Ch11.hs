module Ch11 where

import TicTacToe
import System.Random (randomRIO)
import Data.List (sortOn)

-- 1. Using gametree, verify that there are 549 946 nodes in the complete game tree for a
--    3x3 tic-tac-toe game from the empty grid, and that the maximum depth is 9.
nodes :: Tree a -> Int
nodes (Node _ []) = 1
nodes (Node _ xs) = 1 + sum (map nodes xs)

totalNodes :: Int
totalNodes = nodes $ gametree empty O -- -> 549 946 (takes a while)

-- 2. Modify the final program to choose a random move from the list of best moves, using
--    the function randomRIO.
bestMoves :: Grid -> Player -> [Grid]
bestMoves g p = [g' | Node (g', p') _ <- xs, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) xs = minimax tree

play2' :: Grid -> Player -> IO ()
play2' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      [] -> do
        putStrLn "ERROR: Invalid move"
        play2' g p
      [g'] -> play g' (next p)
  | p == X = do
    putStr "Player X is thinking... "
    let bs = bestMoves g p
    i <- randomRIO (0, length bs - 1)
    play (bs !! i) (next p)

-- 3. Modify the minimax algorithm to take the quickest route to a win by calculating the
--    depths of resulting trees and selecting a move that results in a tree with the
--    smallest depth.
bestMove :: Grid -> Player -> Grid
bestMove g p = head [g' | Node (g', p') _ <- sortOn calcDepth xs, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) xs = minimax tree

calcDepth :: Tree a -> Int
calcDepth (Node _ []) = 0
calcDepth (Node _ xs) = 1 + maximum (map calcDepth xs)
module GameOfLife where

import Control.Concurrent (threadDelay)
import GHC.IO.Exception (ExitCode)
import System.Process (system)

height :: Int
height = 30

width :: Int
width = 30

-- In miliseconds
speed :: Int
speed = 500

type Pos = (Int, Int)

type Grid = [Pos]

-- PRE-BUILT GRIDS --

gliderCol :: Grid
gliderCol = [(0, 2), (1, 0), (1, 2), (2, 1), (2, 2), (3, 11), (4, 9), (4, 10), (5, 10), (5, 11)]

glider :: Grid
glider = [(0, 2), (1, 0), (1, 2), (2, 1), (2, 2)]

ship :: Grid
ship = [(0, 0), (0, 3), (1, 4), (2, 0), (2, 4), (3, 1), (3, 2), (3, 3), (3, 4)]

-- PRE-BUILT GRIDS --

showPos :: Pos -> Grid -> Char
showPos pos grid
  | pos `elem` grid = '0'
  | otherwise = ' '

rowLines :: String -> String
rowLines "" = ""
rowLines gridStr = take width gridStr ++ "\n" ++ rowLines (drop width gridStr)

showGrid :: Grid -> String
showGrid grid =
  rowLines
    [ showPos (r, c) grid | r <- [0 .. (height - 1)], c <- [0 .. (width - 1)]
    ]

neighbors :: Pos -> [Pos]
neighbors (r, c) =
  [ (r - 1, c - 1),
    (r - 1, c),
    (r - 1, c + 1),
    (r, c - 1),
    (r, c + 1),
    (r + 1, c - 1),
    (r + 1, c),
    (r + 1, c + 1)
  ]

livingNeighs :: Pos -> Grid -> Int
livingNeighs pos grid = length $ filter (`elem` grid) (neighbors pos)

survivors :: Grid -> Grid
survivors grid = filter (\x -> livingNeighs x grid `elem` [2, 3]) grid

births :: Grid -> Grid
births grid =
  [ (r, c) | r <- [0 .. (height - 1)], c <- [0 .. (width - 1)], livingNeighs (r, c) grid == 3 && (r, c) `notElem` grid
  ]

nextGen :: Grid -> Grid
nextGen grid = survivors grid ++ births grid

cls :: IO ExitCode
cls = system "cls"

play :: Grid -> IO ()
play grid = do
  cls
  putStr $ showGrid grid
  threadDelay $ speed * 10 ^ 3
  play $ nextGen grid

-- Choose any pre-built grid or customize your own!
main :: IO ()
main = do play gliderCol
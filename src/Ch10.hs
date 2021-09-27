module Ch10 where

-- 1. Redefine putStr using a list comprehension and the library function sequence_.
putStr' :: String -> IO ()
putStr' str = sequence_ [putChar x | x <- str]

-- 2. Using recursion, define a version of putBoard that displays nim boards of any size.
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr $ show row
                    putStr ": "
                    putStrLn $ concat (replicate num "* ")

putBoard :: Board -> IO ()
putBoard xs = displayRowN xs 1

displayRowN :: Board -> Int -> IO ()
displayRowN [] _ = return ()
displayRowN (x:xs) n = do putRow x n
                          displayRowN xs (n+1)

-- 3. Redefine the generalised version of putBoard using a list comprehension and
--    sequence_.
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [displayRowN xs 1]

-- 4. Define an action adder that reads a given number of integers from the keyboard
--    and displays their sum
adder :: Int -> IO ()
adder n = do s <- takeNinputs [] n
             putStrLn $ "Sum is: " ++ show s

takeNinputs :: [Int] -> Int -> IO Int
takeNinputs xs 0 = return (sum xs)
takeNinputs xs n = do a <- readLn
                      takeNinputs (a:xs) (n-1)

-- 5. Redefine adder using the function sequence_.
adder' :: Int -> IO ()
adder' n = sequence_ [takeNinputs [] n]
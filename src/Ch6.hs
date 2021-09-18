module Ch6 where

-- 1. Modify the definition of factorial function to prohibit negative arguments by
--    adding a guard to the recursive case.
factorial :: Int -> Int
factorial 0 = 1
factorial n | n < 0     = n
            | otherwise = n * factorial (n-1)

-- 2. Define sumdown :: Int -> Int that returns the sum of non-negative integers from a
--    given number down to zero.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 3. Definde the exponentiation operator ^ for non-negative numbers using the same
--    pattern of recursion as the multiplication operator *, and show how the expression
--    2^3 is evaluated using your definition.
expo :: Int -> Int -> Int
expo _ 0 = 1
expo n 1 = n
expo n e = n * (n `expo` (e-1))
-- expo 2 3 evaluates as
-- 2 * (2 `expo` (2)) -> 2 * (2 * (2 `expo` (1))) -> 2 * (2 * (2)) which gives 8

-- 4. Define a recursive function euclid :: Int -> Int -> Int that implements Euclid's
--    algorithm for calculating the greatest common divisor of two non-negative integers.
euclid :: Int -> Int -> Int 
euclid n m | r == 0    = b
           | otherwise = euclid b r
           where
               (a,b) = (max n m, min n m)
               r     = a `mod` b

-- 5. Using the recursive definitions given in this chapter, show how length [1,2,3],
--    drop 3 [1,2,3,4,5] and init [1,2,3] are evaluated.
-- length [1,2,3] -> 1 + (length [2,3]) -> 1 + (1 + (length [3])) ->
--   1 + (1 + (1 + (length []))) -> 1 + (1 + (1 + (0))) -> 3

-- drop 3 [1,2,3,4,5] -> drop 2 [2,3,4,5] -> drop 1 [3,4,5] -> drop 0 [4,5] -> [4,5]

-- init [1,2,3] -> 1 : (init [2,3]) -> 1 : (2 : (init [3])) -> 1 : (2 : ([])) -> [1,2]

-- 6. Define the following functions on lists using recursion:
and' :: [Bool] -> Bool
and' []        = True
and' (False:_) = False
and' (True:bs) = and' bs

concat' :: [[a]] -> [a]
concat' []       = []
concat' [xs]     = xs
concat' (as:bss) = as ++ concat' bss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- !!
select :: [a] -> Int -> a
select (x:_) 0  = x
select (_:xs) n = select xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' x (y:ys) | x == y = True
               | otherwise = elem' x ys

-- 7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two
--    sorted lists to give a single sorted list.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8. Using merge, define a function msort :: Ord a => [a] -> [a] that sorts a list by
--    merging its halves. Use halve :: [a] -> ([a],[a]) for this.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort h1) (msort h2)
        where
            (h1,h2) = halve xs

-- 9. Using the five-step process, construct functions that:
-- a. calculate the sum of a list of numbers
sum' :: [Int] -> Int
sum' []     = 0
sum' (n:ns) = n + sum' ns

-- b. take a given number of elements from the start of a list
take' :: Int -> [a] -> [a]
take' _ []     = []
take' 0 xs     = xs
take' n (x:xs) = take' (n-1) xs

-- c. select the last element of a non-empty list
last' :: [a] -> a
last' [x]    = x
last' (_:xs) = last' xs
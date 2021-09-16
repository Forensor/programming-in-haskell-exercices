module Ch3 where

-- 1. What are the types of the following values?
-- ['a','b','c']             :: [Char]
-- ('a','b','c')             :: (Char, Char, Char)
-- [(False,'0'),(True,'1')]  :: [(Bool, Char)]
-- ([False,True], ['0','1']) :: ([Bool], [Char])
-- [tail,init,reverse]       :: [([a] -> [a])]

-- 2. Write down definitons that have the following types:
bools :: [Bool]
bools = [True,False, True]

nums :: [[Int]]
nums = [[1,2,3],[1],[]]

add :: Int -> Int -> Int -> Int
add x y z = x+y+z

copy :: a -> (a, a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f = f

-- 3. What are the types of the following functions?
second :: [a] -> a
second xs = head (tail xs)

swap :: (b, a) -> (a, b)
swap (x,y) = (y,x)

pair :: a -> b -> (a, b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 4. Check your answers to the preceding three questions using GHCi.
-- ☝️

-- 5. Why is it not feasible in general for function types to be instances of the Eq
--    class? When is it feasible?
-- Comparing functions seems redundant and rather unnatural, I could only think it's
-- feasible when both functions return the same results given the same arguments.
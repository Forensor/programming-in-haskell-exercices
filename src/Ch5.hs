module Ch5 where

import Data.Char
import Data.List

-- 1. Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 +
--    ...100^2 of the first one hunred integer squares.
expr :: Integer
expr = sum [n^2 | n <- [1..100]]

-- 2. Using a list comprehension, define a function grid :: Int -> Int -> [(Int, Int)]
--    that returns a coordinate grid of a given size.
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(r,c) | r <- [0..x], c <- [0..y]]

-- 3. Using a list comprehension and the function grid above, define a function
--    square :: Int -> [(Int, Int)] that returns a coordinate square of size n, excluding
--    the diagonal from (0,0) to (n,n).
square :: Int -> [(Int, Int)]
square n = [(r,c) | (r,c) <- grid n n, r /= c]

-- 4. In a similar way to the function length, show how replicate :: Int -> a -> [a] can
--    be defined using a list comprehension.
replicate' :: Int -> a -> [a]
replicate' n x = [e | (e, n') <- zip (repeat x) [0..n], n' /= n]
-- Although:
replicate2' n x = [x | _ <- [1..n]]
-- is more idiomatic

-- 5. A triple (x,y,z) of positive integers is Pythagorean if it satisfies the equation
--    x^2+y^2 = z^2. Using a list comprehension with three generators, define a function
--    pyths :: Int -> [(Int, Int, Int)] that returns the list of all such triples whose
--    componentes are at most a given limit.
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 6. A positive integer is perfect if it equals the sum of all of its factors, excluding
--    the number itself. Using list comprehension and the function factors, define a 
--    function perfects :: Int -> [Int] that returns the list of all perfect numbers up
--    to a given limit.
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (init (factors x)) == x]

-- 7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two
--    generators can be re-expressed using two comprehensions with single generators.
expr' :: [(Integer, Integer)]
expr' = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8. Redefine the function positions using the function find.
find' :: Eq a => a -> [(a, b)] -> [b]
find' k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find' x (zip xs [0..])
-- instead of: positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- 9. Define a function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar
--    product of two list using a list comprehension.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 0. Modify the Caesar cipher to also handle upper-case letters.
chr2int :: Char -> Int
chr2int c = ord c - ord 'a'

int2chr :: Int -> Char
int2chr n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c | isLetter c = int2chr ((chr2int (toLower c) + n) `mod` 26)
          | otherwise  = c

cipher :: Int -> String -> String
cipher n xs = [shift n x | x <- xs]

-- Frequencies of english alphabet letters in the majority of texts
letFreq :: [Float]
letFreq = [8.1,1.5,2.8,4.2,12.7,2.2,2.0,6.1,7.0,
           0.2,0.8,4.0,2.4,6.7,7.5,1.9,0.1,6.0,
           6.3,9.0,2.8,1.0,2.4,0.2,2.0,0.1]

percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

calcStrLetFreq :: String -> [Float]
calcStrLetFreq xs = [percent (count x (map toLower xs)) n | x <- ['a'..'z']]
                 where n = length (filter isLetter xs)

-- Chi-square statistic, a method to compare a list of observed freqs with a list of
-- expected ones.
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

decipher :: String -> String
decipher xs = cipher (-factor) xs
              where
                  factor   = head (positions (minimum chitab) chitab)
                  chitab   = [chisqr (rotate n letFreq') letFreq | n <- [0..25]]
                  letFreq' = calcStrLetFreq xs
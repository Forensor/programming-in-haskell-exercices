module Ch4 where

-- 1. Using library functions, define a function halve :: [a] -> ([a], [a]) that splits
--    an even list into two halves.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

-- 2. Define a function third :: [a] -> a that returns the third element in a list that
--    contains at least this many elements using:
-- a. head and tail
third :: [a] -> a
third = head . tail . tail

-- b. list indexing !!
third' :: [a] -> a
third' = (!!2)

-- c. pattern matching
third2' :: [a] -> a
third2' (_:_:x:_) = x

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail
--    except that it maps the empty list to itself rather than producing an error. Using
--    tail and the function null :: [a] -> Bool that decides if a list is empty or not,
--    define safetail using:
-- a. a conditional expresion
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

-- b. guarded equations
safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

-- c. pattern matching
safetail2' :: [a] -> [a]
safetail2' [] = []
safetail2' xs = tail xs

-- 4. Show how the disjunction operator || can be defined in four different ways using
--    pattern matching.
(||) :: Bool -> Bool -> Bool
True || True = True
True || _    = True
_ || True    = True
_ || _       = False

-- 5. Show how the meaning of the following pattern matching definition for logical
--    conjunction && can be formalised using conditional expressions. Use two nested
--    conditional expressions.
(&&) :: Bool -> Bool -> Bool
a && b = if a == False then False else
            if b == False then False else True

-- 6. Do the same for the following alternative definiton:
-- True && b  = b
-- False && _ =  False
and' :: Bool -> Bool -> Bool
a `and'` b = if a == True then b else False

-- 7. Show how the meaning of the following curried function definition can be formalised
--    in terms of lambda expressions:
-- mult :: Int -> Int -> Int -> Int
-- mult x y z = x*y*z
mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))

-- 8. The Luhn algorithm is used to check bank card numbers for simple errors such as
--    mistyping a digit, and proceeds as follows:

-- · consider each digit as a separate number
-- · moving left, double every other number from the second last
-- · substract 9 from each number that is now greater than 9
-- · add all the resulting numbers together
-- · if the total is divisible by 10, the card number is valid

-- Define a function luhnDouble :: Int -> Int that doubles a digit and substracts 9 if it
-- is greater then 9.
luhnDouble :: Int -> Int
luhnDouble n | double > 9 = double - 9
             | otherwise  = double
             where
                 double = n*2

-- Using luhnDouble and the integer remainder function mod, define a function
-- luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four digit bank card number
-- is valid.
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (==0) 
             $ (`mod` 10) 
             $ sum 
             $ zipWith ($) (cycle [luhnDouble, id]) [a,b,c,d]
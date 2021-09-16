module Ch2 where

-- 1. Work through the examples from this chapter using GHCi.
-- average [1,2,3,4,5] -> 3
-- take (double 2) [1,2,3,4,5] -> [1,2,3,4]
-- ...

-- 2. Parenthesise the numeric expressions:
-- (2^3)*4
-- (2*3)+(4*5)
-- 2+(3*(4^5))

-- 3. Correct the errors from the script and then check it with GHCi.
n :: Int
n = a `div` length xs
    where
        a  = 10
        xs = [1,2,3,4,5]

-- 4. Show how last could be defined in terms of the other library functions introduced
--    in this chapter. Can you think of another possible definition?
last' :: [a] -> a
last' = head . reverse

-- 5. Show how init could be defined in two different ways.
init' :: [a] -> [a]
init' xs = take (length xs - 1) xs
init2' :: [a] -> [a]
init2' = reverse . tail . reverse
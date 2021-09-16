module Ch1 where

-- 1. Give another possible calculation for the result of double (double 2).
-- (double 2) + (double 2)

-- 2. Show that sum [x] = x for any number x.
-- sum [x] -> x + (sum []) -> x + 0, so x remains the same.

-- 3. Define a function that produces the product of a list of numbers.
prod :: Num p => [p] -> p
prod []     = 1
prod (n:ns) = n * prod ns

-- 4. How should the definition of qsort be modified so that it produces a reverse sorted
--    version of a list?
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort greater ++ [x] ++ qsort lesser
               where
                   greater = [a | a <- xs, a >= x]
                   lesser  = [b | b <- xs, b < x]

-- 5. What would be the effect of replacing <= by < in the original definition of qsort?
-- Repeated numbers will be discarded, remaining only one of them
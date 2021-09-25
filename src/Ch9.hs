module Ch9 where

-- 1. Redefine the combinatorial function choices using a list comprehension rather than
--    using composition, concat and map.
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms = foldr (concatMap . interleave) [[]]

choices :: [a] -> [[a]]
choices ns = [x | x <- concat (map perms (subs ns))]

-- 2. Define a recursive function isChoice that decides if one list is chosen from
--    another, without using the functions perms and subs.
removeFst :: Eq a => a -> [a] -> [a]
removeFst x [] = []
removeFst x (y:ys) = case x == y of
                         True -> ys
                         _    -> y : removeFst x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFst x ys)

-- 3. What effect would generalising the function split to also return pairs containing
--    the empty list have on the behaivour of solutions.
-- It would cause infinite loops because the length of the list wouldn't be reduced.

-- 4. Using the functions provided, verify that there are 33 665 406 possible expressions
--    over the numbers 1, 3, 7, 10, 25, 50, and that only 4 672 540 of these expressions
--    evaluate successfully.
data Op
  = Add
  | Sub
  | Mul
  | Div

data Expr
  = Val Int
  | App Op Expr Expr

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns =
  [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- [Add, Sub, Mul, Div]]

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- These two below take a while to evaluate.

expressions :: Int
expressions = length [x | ns' <- choices [1, 3, 7, 10, 25, 50],
                            e <- exprs ns', 
                            x <- eval e] -- -> 33 665 406

successful :: Int
successful = length [x | ns' <- choices [1, 3, 7, 10, 25, 50],
                           x <- exprs ns'] -- -> 4 672 540
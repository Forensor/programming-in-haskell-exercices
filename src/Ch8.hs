module Ch8 where

-- 1. In a similar manner to the function add, define a recursive multiplication function
--    mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (m `add` n)

mult :: Nat -> Nat -> Nat
mult _ Zero     = Zero
mult m (Succ n) = m `add` (m `mult` n)

-- 2. Using the library function compare, redefine the function occurs for search trees.
--    Why is this new definition more efficient than the original version?
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) | c == EQ   = True
                      | c == LT   = occurs x l
                      | otherwise = occurs x r
                      where c = compare x y

-- Is more efficient because it only goes through one path in the entire tree instead of
-- checking all branches.

-- 3. Define a function balanced :: Tree a -> Bool that decides if a binary tree is
--    balanced or not.
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)

leaves :: Tree2 a -> Int
leaves (Leaf2 _)     = 1
leaves (Node2 l r) = leaves l + leaves r

balanced :: Tree2 a -> Bool
balanced (Leaf2 _) = True
balanced (Node2 l r)   = leaves l == leaves r 
                      || leaves l == leaves r - 1
                      || leaves l == leaves r + 1

-- 4. Define a function balance :: [a] -> Tree a that converts a non-empty list into a
--    balanced tree.
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs = Node2 (balance h1) (balance h2)
             where (h1, h2) = halve xs

-- 5. Given the type declaration:
data Expr = Val Int | Add Expr Expr
--    define a higher-order function 
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

--    such that folde fg replaces Val by f and Add by g.
folde f g (Val n) = f n
folde f g (Add a b) = g (folde f g a) (folde f g b)

-- 6. Using folde, define a function eval that evaluates an Expr to an integer value, and
--    size that calculates the number of values in an expression.
eval :: Expr -> Int
eval = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 7. Complete the following instance declarations:
instance Eq a => Eq (Maybe a) where
    Nothing == Nothing   = True
    (Just a) == (Just b) = a == b
    _ == _               = False

instance Eq a => Eq [a] where
    [] == []         = True
    [a] == [b]       = a == b
    (a:as) == (b:bs) = a == b && as == bs
    _ == _           = False
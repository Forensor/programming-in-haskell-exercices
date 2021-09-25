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
--instance Eq a => Eq (Maybe a) where
--    Nothing == Nothing   = True
--    (Just a) == (Just b) = a == b
--    _ == _               = False

--instance Eq a => Eq [a] where
--    [] == []         = True
--    [a] == [b]       = a == b
--    (a:as) == (b:bs) = a == b && as == bs
--    _ == _           = False

-- 8. Extend the tautology checker to support the use of logical disjunction and
--    equivalence in propositions.

data Prop
  = Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Imply Prop Prop
  | Equiv Prop Prop

type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

type Subst = Assoc Char Bool

eval' :: Subst -> Prop -> Bool
eval' _ (Const b)   = b
eval' s (Var x)     = find x s
eval' s (Not p)     = not (eval' s p)
eval' s (And p q)   = eval' s p && eval' s q
eval' s (Or p q)    = eval' s p || eval' s q
eval' s (Imply p q) = eval' s p <= eval' s q
eval' s (Equiv p q) = eval' s p == eval' s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False :) bss ++ map (True :) bss
          where
              bss = bools (n - 1)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where
               vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval' s p | s <- substs p]
module Ch14 where

import Data.Foldable

-- 1. Complete the following instance declaration from Data.Monoid to make a pair type
--    into a monoid provided the two component types are monoids:
-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--     mempty = (mempty, mempty)
--     (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)

-- 2. In a similar manner, show how a function type a -> b can be made into a monoid
--    provided that the result type b is a monoid.
-- instance Monoid b => Monoid (a -> b) where
--     mempty = const mempty
--     fl `mappend` fr = \x -> fl x `mappend` fr x

-- 3. Show how the Maybe type can be made foldable and traversable, by giving explicit 
--    definitions for fold, foldMap, foldr, foldl and traverse.
-- instance Foldable Maybe where
--     fold Nothing = mempty
--     fold (Just a) = a

--     foldMap _ Nothing = mempty
--     foldMap f (Just a) = f a

--     foldr _ _ Nothing = mempty
--     foldr f v (Just a) = f a v

--     foldl _ _ Nothing = mempty
--     foldl f v (Just b) = f v b

-- instance Traversable Maybe where
--     traverse g Nothing = pure Nothing
--     traverse g (Just a) = Just <$> g a

-- 4. In a similar manner, show how the following type of binary trees with data in their
--    nodes can be made into a foldable and traversable type:
data Tree a
    = Leaf
    | Node (Tree a) a (Tree a)
    deriving (Show)

instance Functor Tree where
    fmap g Leaf = Leaf
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

instance Foldable Tree where
    fold Leaf = mempty
    fold (Node l a r) = fold l `mappend` a `mappend` fold r

    foldMap _ Leaf = mempty
    foldMap f (Node l a r) = foldMap f l `mappend` f a `mappend` foldMap f r

    foldr _ v Leaf = v
    foldr f v (Node l a r) = foldr f (foldr f (f a v) r) l

    foldl _ v Leaf = v
    foldl f v (Node l b r) = foldl f (foldl f (f v b) l) r

instance Traversable Tree where
    traverse g Leaf = pure Leaf
    traverse g (Node l a r) = Node <$> traverse g l <*> g a <*> traverse g r

-- 5. Using foldMap, define a generic version of the higher-order function filter on 
--    lists that can be used with any foldable type:
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF f = foldMap (\x -> if f x then [x] else mempty)
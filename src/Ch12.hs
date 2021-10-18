module Ch12 where

import System.Directory.Internal.Prelude (Functor)
import GHC.HsToCore.Monad (Applicative)

-- 1. Define an instance of the Functor class for the following type of binary trees that
--    have data in their nodes:
data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

instance Functor Tree where
    fmap _ Leaf         = Leaf
    fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)

-- 2. Complete the following instance declaration to make the partially-applied function
--    type (a ->) into a functor:
-- instance Functor ((->) a) where
--     fmap = (.)

-- 3. Define an instance of the Applicative class for the type ((->) a).
-- instance Applicative ((->) a) where
--     pure = const
--     g <*> h = \ x -> g x (h x)

-- 4. There may be more than one way to make a parameterised type into an applicative
--    functor. For example, the library Control.Applicative provides an alternative
--    'zippy' instance for lists, in which the function pure makes an infinite list of
--    copies of its argument, and the operator <*> applies each argument function to the
--    corresponding argument value at the same position. Complete the following
--    declarations that implement this idea:
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

-- 5. Work out the types for the variables in the four applicative laws.
-- pure id <*> x   = x                            :: Applicative f => f b -> f b
-- pure (g x)      = pure g <*> pure x            :: Applicative f => (t -> a) -> t -> f a
-- x <*> pure y    = pure (\g -> g y) <*> x       :: Applicative f => f (a -> b) -> a -> f b
-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z :: Applicative f => f (a1 -> b) -> f (a2 -> a1) -> f a2 -> f b

-- 6. Define an instance of the Monad class for the type (a ->).
-- instance Monad ((->) a) where
--     return = const
--     (>>=) f g r = g (f r) r

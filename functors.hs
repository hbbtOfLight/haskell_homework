module Functors (Pair, Labelled, OneOrTwo, MEither, MultiTree, Stream) where
data Pair a = Pair a a deriving(Show)
data Labelled e a = Labelled e a deriving(Show)
data OneOrTwo a = One a | Two a a deriving(Show)
data MEither e a = MLeft e | MRight a deriving(Show)
data MultiTree a = Leaf | Node a [MultiTree a] deriving(Show)
data Stream a = Cons a (Stream a) deriving(Show)

-- Наверное здесь должно применяться к обоим элементам нечто
instance Functor Pair where
    fmap f (Pair a b) = (Pair (f a) (f b))

--   А здесь логично ко 2-му
instance Functor (Labelled e) where
    fmap f (Labelled e a) = Labelled e (f a)

instance Functor OneOrTwo where
    fmap f (One a ) = (One (f a) )
    fmap f (Two a b) = Two (f a) (f b)

instance Functor (MEither e) where
    fmap _ (MLeft e)  = MLeft e
    fmap f (MRight a) = MRight (f a)
    
instance Functor MultiTree where
    fmap _ Leaf = Leaf
    fmap f (Node a subtrees) = Node (f a) (map (fmap f) subtrees)

instance Functor Stream where
    fmap f (Cons a b) = Cons (f a) (fmap f b)


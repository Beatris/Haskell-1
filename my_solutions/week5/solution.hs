--01. The Binary tree functor
data BTree a = Node a (BTree a) (BTree a) | Leaf a | Nil
  deriving (Show)

instance Functor BTree where
--fmap (a -> b) -> f a -> f b
  fmap f Nil = Nil
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Node a b c) = Node (f a) (fmap f b) (fmap f c)

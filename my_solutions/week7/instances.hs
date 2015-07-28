-- 01. Instances
instance Monad Maybe where
  return = Just
  Just a  >>= f = f a
  Nothing >>= _ = Nothing

instance  Monad List where
  return x = Cons x Nil
  Nil       >>= f = Nil
  Cons x xs >>= f = f x `appendList` (xs >>= f)

instance  Monad ((->) a) where
--return :: c -> (a -> c)
--(>>=)  :: (a -> c) -> (c -> (a -> b)) -> (a -> b)
  return    = const
  f (>>=) g = \a -> g (f a) a
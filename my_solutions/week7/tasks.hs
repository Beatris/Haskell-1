--02. join
join :: Monad m => m (m a) -> m a
join m = m >>= id

--03. sequenceM
sequenceM :: Monad m => [m a] -> m [a]
sequenceM (f:fs) = f >>= \a ->
  sequenceM fs >>= return . (a:)
sequenceM []     = return []

--04. replicateM
replicateM :: Monad m => Int -> m a -> m [a]
replicateM x m = sequence $ replicate x m

--05. filterM
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = return []
filterM p (x:xs) = p x >>= \b ->
  if b then filterM p xs >>= \as -> return $ x : as
       else filterM p xs

--06. mapMM
mapMM :: Monad m => (a -> m b) -> [a] -> m [b]

--(>>=)  :: m a -> (a -> m b) -> m b
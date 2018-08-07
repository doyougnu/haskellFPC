module MaybeT where

import Control.Monad.Trans
import Control.Monad (liftM)

-- | We want to define a maybe transformer monad
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- | this is what the bind would look like. Notice the inner do statement
-- and the fact that we need to wrap it in our MaybeT constructor
bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y -> runMaybeT (f y)

-- | This is a more idiomatic version of bindMT from above
x `altBindMT` f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

-- | This is what a return will look like for our maybeT
returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

-- | and failure
failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

instance (Functor m) => Functor (MaybeT m) where
  fmap f = MaybeT . (fmap (fmap f)) . runMaybeT

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure = MaybeT . return . Just

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT
  fail = failMT

-- | Now we can make an instance of MonadTrans to get the "lift" function
instance MonadTrans MaybeT where
  lift m = MaybeT (Just `liftM` m)

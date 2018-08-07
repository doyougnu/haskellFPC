module Reader where

import Control.Monad.Reader as R
-- You must enable FunctionalDependecies for this syntax

-- This is saying that for the type MonadReader for some monad m, and some type
-- r, r is uniquely determined by m. There is a long history here. Just google
-- mtl vs transformers

-- In the mtl library all monad type classes follow the same pattern as
-- MonadReader below. e.g. there is MonadWriter, MonadState &c. Then for each
-- monad there will be a transformer type classe name MonadReaderT. The naming
-- is only done by convention
class (Monad m) => MonadReader r m | m -> r where
    ask   :: m r
    local :: (r -> r) -> m a -> m a

myName step = do
  name <- R.ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- R.local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)

-- and again we can run this with runReader like so:
  -- R.runReader localExample "El Jefe"

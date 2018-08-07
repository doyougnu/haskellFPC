module UglyStack where

import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      stDeepestReached :: Int
    } deriving (Show)

-- | Look we can stack! And it is even eta-reduced!
type App = ReaderT AppConfig (StateT AppState IO)

-- here is the explicit version:
-- type App a = ReaderT AppConfig (StateT AppState IO) a

-- and to execute the monad transformer stack we need to unwrap the onion
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT k config) state

-- | now our constrained count function will look like this
constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
            let newPath = path </> name
            isDir <- liftIO $ doesDirectoryExist newPath
            if isDir && curDepth < cfgMaxDepth cfg
              then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                  put st { stDeepestReached = newDepth }
                constrainedCount newDepth newPath
              else return []
  return $ (path, length contents) : concat rest

-- Here is an example of how one would really write this in normal haskell code
-- we want to provide an api, and hide the fact that we are really using a monad
-- transformer stack

-- Such magic requires the GeneralizedNewTypeDeriving extension
-- newtype MyApp a = MyA {
--       runA :: ReaderT AppConfig (StateT AppState IO) a
--     } deriving (Functor,Applicative,Monad,MonadIO,MonadReader AppConfig,
--                 MonadState AppState)

-- The "magic" going on here is that the toplevel monad i.e. the Reader monad
-- will be an instance of the mtl type classes like monadState,
-- so when we call a "get" or a "put", we'll lift appropriately to get to the
-- top level

-- runMyApp :: MyApp a -> Int -> IO (a, AppState)
-- runMyApp k maxDepth =
--     let config = AppConfig maxDepth
--         state = AppState 0
--     in runStateT (runReaderT (runA k) config) state

-- -- | we can get the state from the inner state monad
-- implicitGet :: App AppState
-- implicitGet = get

-- -- | we can also "lift" the get function from the inner state monad to the top
-- -- level reader monad
-- explicitGet :: App AppState
-- explicitGet = lift get

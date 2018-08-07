module CountEntriesT ( listDirectory
                     , countEntries
                     ) where

import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_, when, liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (WriterT, tell, runWriterT, execWriterT, runWriter, execWriter)


-- | The same function with a writerT monad transformer
-- general type is Writer w a; where w is the writer, and a is the result type
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  -- notice this liftIO function This lifts IO actions into the Writer monad
  -- because monad transformer stacks are onions!
  contents <- liftIO . listDirectory $ path
  -- tell writes the the writer monad
  tell [(path, length contents)]

  -- forM_ is a flip of the monadic fmap, mapM
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

-- Now we use run or exec WriterT to "run the monad"
-- like this:
  -- :t runWriterT
  --    runWriterT :: WriterT w m a -> m (a, w)

  -- :t execWriterT
  --    execWriterT :: Monad m => WriterT w m a -> m w

  -- :t execWriterT $ countEntries ".."
  -- execWriterT $ countEntries ".." :: IO [(FilePath, Int)]

  -- error! take 4 $ execWriterT $ countEntries ".."

  -- we need to lift into monads like this:

  -- take 4 `liftM` execWriterT (countEntries "..")

  -- or

  -- (liftM $ take 4) $ execWriterT (countEntries "..")

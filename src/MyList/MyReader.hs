module MyList.MyReader where

import Control.Monad.Reader

-- First lets get weird
-- What is the monad that this do notation is using?
foo :: Int -> Int
foo = do
  a <- (*1729)
  b <- (+20)
  return $ a + b

-- Does this help?
foo' :: Int -> Int
foo' x = let a = (*1729) x
             b = (+20) x
         in a + b

-- A: Its the function arrow itself! we are basically treating the input value
-- as a "context". This forms the basis for the Reader Monad

-- Let's be more explicit
type MyCtx = Int

-- Reader is also called the "Environment Monad"
type MyReader = Reader Int

foo'' :: MyReader Int
foo'' = do myVal <- ask
           let a = (*1729) myVal
               b = (+20) myVal
           return $ a + b

-- Why won't this work?
-- foo''' :: MyReader Int
-- foo''' = do myVal <- ask
--             a <- (*1729) myVal
--             b <- (+20) myVal
--             return $ a + b

-- That's it the Reader monad is a super simple monad. Here is something you
-- might see in a real app

data MyCtx' = MyCtx' { feature1 :: Bool
                     , feature2 :: Bool
                     , aLog :: String
                     }

sampleCtx = MyCtx' {feature1=False, feature2=False, aLog="The beginning of log"}

nonMonadic :: MyCtx' -> String
nonMonadic x@MyCtx'{..} = if f1 then "F1 was true" else aLog
  where f1 = feature1
        f2 = feature2

bar :: Reader MyCtx' String
bar = do f1 <- asks feature1
         f2 <- asks feature2
         aLog <- asks aLog
         let result = if f1
               then "F1 was true"
               else aLog
         return result

-- run like: runReader bar (MyCtx' {feature1=False, feature2=False, aLog=""})

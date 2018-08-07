module MyList.MyWriter where

import Control.Monad.Writer

nonMonadic :: (String, Int) -> (String, Int)
nonMonadic (log,x) = (log ++ "Increasing x!", succ x)

nonMonadic' :: Int -> (String, Bool)
nonMonadic' x = if res
                then ("x failed the test!", res)
                else ("x was greater than 3!", res)
  where res = x < 3

type MyLoggingApp = Writer String

foo :: Int -> MyLoggingApp Int
foo x = do let newX = x * 200
           tell $ "ok I've multiplied " ++ show x ++ " by 200"
           return newX

foo' :: MyLoggingApp ()
foo' = tell "one" >> tell "two" >> tell "three"

foo'' :: MyLoggingApp ()
foo'' = do _ <- tell "one"
           _ <- tell "two"
           _ <- tell "three"
           return ()


bar ::  Int -> MyLoggingApp Int
bar x = do foo'
           foo x

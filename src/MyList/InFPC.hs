module MyList.InFPC where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
-- import Control.Monad (filterM)


example xs f = do a <- xs
                  return $ f a

ex2 :: Maybe a -> (a -> b) -> Maybe b
ex2 x f = do x' <- x
             let x'' = x
                 x''' = x
             return $ f x'

example' xs f = xs >>= return . f

example3 xs f = [f x | x <- xs]

example4 xs ys = do x <- xs
                    y <- ys
                    guard $ x /= y
                    return (x, y)

example4 xs ys = [ (x, y) | x <- xs, y <- ys, x /= y ]

type KnightPos = (Int, Int)

possibleMoves :: KnightPos -> [KnightPos]
possibleMoves (col, row) = [ (col + 2, row - 1)
                           , (col + 2, row + 1)
                           , (col - 2, row - 1)
                           , (col - 2, row + 1)
                           , (col + 1, row - 2)
                           , (col + 1, row + 2)
                           , (col - 1, row - 2)
                           , (col - 1, row + 2)
                           ]

isOnBoard :: KnightPos -> Bool
isOnBoard (col, row) = col `elem` [1..8] &&
                       row `elem` [1..8]

-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight pos = do new

-- foo pos = zip
--   where x = possibleMoves pos
--         y = isOnBoard x

moveKnight pos = do x <- possibleMoves pos
                    guard $ isOnBoard x
                    return x

in3 :: KnightPos -> [KnightPos]
in3 pos = do xs <- moveKnight pos
             ys <- moveKnight xs
             moveKnight ys

in3' :: KnightPos -> [KnightPos]
in3' pos = return pos >>= moveKnight >>= moveKnight >>= moveKnight


foo :: Int -> Int
foo = do a <- (*1729)
         b <- (+20)
         return $ a + b

foo' :: Int -> Int
foo' x = let a = (*1729) x
             b = (+20) x
         in a + b

foo'' :: Reader Int Int
foo'' = do myVal <- ask
           let a = (*1729) myVal
               b = (+20) myVal
           return $ a + b


foo''' :: Reader Bool Int
foo''' = do myVal <- ask
            return $ if myVal then 1 else 2

data MyCtx = MyCtx { feature1 :: Bool
                   , feature2 :: Bool
                   , aLog :: String
                   }

type MyReader a = Reader MyCtx a

bar :: MyReader String
bar = do f1 <- asks feature1
         f2 <- asks feature2
         l <- asks aLog
         return $ if f1 then "F1 was true" else l

nonMonadic :: (String, Int) -> (String, Int)
nonMonadic (log, x) = (log ++ "Inc x!", succ x)

type MyLoggingApp a = Writer String a

baz :: Int -> MyLoggingApp Int
baz x = do let newX = x * 200
           tell $ "ok I've multiplied " ++ show x ++ " by 200!"
           return newX

baz' :: MyLoggingApp ()
baz' = tell "one" >>= \_ -> tell "two" >>= \_ -> tell "three"
-- baz' = tell "one" >> tell "two" >> tell "three"

baz'' x = do baz x
             baz'

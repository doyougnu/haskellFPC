module MyList.MyList where

import Control.Monad (filterM, guard)

-- the list monad [], models non-determinism
-- :i []
-- data [] a = [] | a : [a]
--      -- Defined in `ghc-prim-0.5.1.1:GHC.Types'
-- instance Applicative [] -- Defined in `GHC.Base'
-- instance Eq a => Eq [a]
--   -- Defined in `ghc-prim-0.5.1.1:GHC.Classes'
-- instance Functor [] -- Defined in `GHC.Base'
-- instance Monad [] -- Defined in `GHC.Base'
-- instance Monoid [a] -- Defined in `GHC.Base'
-- instance Ord a => Ord [a]
--   -- Defined in `ghc-prim-0.5.1.1:GHC.Classes'
-- instance Show a => Show [a] -- Defined in `GHC.Show'
-- instance Read a => Read [a] -- Defined in `GHC.Read'
-- instance Foldable [] -- Defined in `Data.Foldable'
-- instance Traversable [] -- Defined in `Data.Traversable'

-- | what is the difference between this and map?
-- example :: [Int] -> _
example xs f = do a <- xs
                  return $ f a
example xs f = xs >>= return . f

-- | Perhaps that seems similar to this.
-- List Comprehension is just the list monad!
example2 xs f = [ f a | a <- xs ]

-- | this is how you would write the equivalent list comprehension using do
-- notation

example3' xs ys = [(x, y) | x <- xs, y <- ys, x /= y]

example3 xs ys = do x <- xs
                    y <- ys
                    guard $ x /= y
                    return (x, y)

-- | But the list monad also models non determinism. Lets do an example from
-- LYAH

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

isOnBoard (col, row) = col `elem` [1..8] && row `elem` [1..8]

-- | We can do this without the list monad just for kicks
moveKnight :: KnightPos -> [KnightPos]
moveKnight pos = filter isOnBoard  . possibleMoves $ pos
-- moveKnight = filter isOnBoard  . possibleMoves

moveKnight' :: KnightPos -> [KnightPos]
moveKnight' pos = do newPos <- possibleMoves pos
                     guard $ isOnBoard newPos
                     return newPos

-- | where can we get in 3 moves?
in3 :: KnightPos -> [KnightPos]
in3 start = do first <- moveKnight start
               second <- moveKnight first
               moveKnight second

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- | now we can write a predicate to see where we can go in 3 moves
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 st end = end `elem` in3 st

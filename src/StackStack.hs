module StackStack where

import Control.Monad.State
import Control.Monad.Reader

-- | We must use explicit lifting when dealing with a monad stack such as this
type StackStack = StateT Int (State String)

outerPut :: Int -> StackStack ()
outerPut = put

innerPut :: String -> StackStack ()
innerPut = lift . put

-- If we have a more complex stack and need to access a monad very far down then
-- we must lift lift lift
type Bar = ReaderT Bool StackStack

barPut :: String -> Bar ()
barPut = lift . lift . put

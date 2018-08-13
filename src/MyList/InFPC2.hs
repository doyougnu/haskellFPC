module MyList.InFPC2 where

import Control.Monad.State
import Control.Monad (liftM2)
import qualified Data.Map as M

-- | Variable names.
type Var = String

-- | Abstract syntax.
data Exp = Lit Int          -- integer literal
         | Add Exp Exp      -- addition expression
         | Let Var Exp Exp  -- variable binding
         | Ref Var          -- variable reference
         | Fun Var Exp      -- anonymous function w/ one argument
         | App Exp Exp      -- function application
  deriving (Eq,Show)

-- | Values.
data DVal = DI Int      -- integers
          | DF Var Exp  -- functions
  deriving (Eq,Show)

type Env = M.Map Var
-- type Env a = M.Map Var a

-- and now we would just write sem like this mapping variables to DVals:
-- | Semantic function.
dsem :: Exp -> Env DVal -> Maybe DVal
dsem (Lit i) _ = Just $ DI i
dsem (Add l r) st = case (dsem l st, dsem r st) of
                      (Just (DI i), Just (DI j)) -> Just . DI $ i + j
                      _                          -> Nothing
dsem (Let x e in') st = case dsem e st of
                       Just v -> dsem in' (M.insert x v st)
                       _ -> Nothing
dsem (Ref x)     m = M.lookup x m
dsem (Fun x e)   _ = Just (DF x e)
dsem (App l r)   m = case (dsem l m, dsem r m) of
                       (Just (DF x e), Just v) -> dsem e (M.insert x v m)
                       _ -> Nothing


type Reg = Integer


type MyState a = State Reg a

inc :: MyState ()
inc = do st <- get
         put $ succ st

inc' :: MyState ()
inc' = modify succ

extractAndAdd2 :: MyState Int
extractAndAdd2 = do st <- get
                    put . fromIntegral $ st + 2
                    return (fromIntegral $ st + 2)

test :: MyState ()
test = do inc
          inc
          inc

test2 :: MyState Int
test2 = do inc
           inc
           inc
           extractAndAdd2

test3 :: MyState Int
test3 = do inc
           inc
           inc
           return 5


stateLoop :: a -> MyState a
stateLoop x = do st <- get
                 inc'
                 if st >= 10
                 then return x
                 else stateLoop x

stateLoop2 :: a -> MyState a
stateLoop2 x = do st <- get
                  if st >= 10
                    then return x
                    else do inc'
                            stateLoop2 x

-- Lets rewrite this with a state monad like so
type BadEnv a = State (Env DVal) (Maybe a)
type Env' a = StateT (Env DVal) Maybe a
-- type Env' a b = StateT (Env DVal) (Either a) b
-- type T b = Env' String  b
-- type Env' a = StateT (Env DVal) Maybe a

-- dsem'' :: Exp -> BadEnv DVal
-- dsem'' (Lit i) = return . Just $ DI i
-- dsem'' (Add l r) = do (Just (DI l'')) <- dsem'' l
--                       (Just (DI r'')) <- dsem'' r
--                       return . Just . DI $ l'' + r''
-- dsem'' (Let x b e) = do -- st <- get
--                        res <- dsem'' b
--                        modify $ M.insert x res
--                        -- put $ M.insert x res st
--                        dsem'' e
-- dsem'' (Ref x) = do st <- get
--                     lift $ M.lookup x st
-- dsem'' (Fun x e)   = return $ (DF x e)
-- dsem'' (App l r)   = do st <- get
--                         (DF x e) <- dsem'' l
--                         v <- dsem'' r
--                         modify $ M.insert x v
--                         dsem'' e

dsem' :: Exp -> Env' DVal
dsem' (Lit i) = return $ DI i
dsem' (Add l r) = do DI l' <- dsem' l
                     DI r' <- dsem' r
                     return . DI $ l' + r'
dsem' (Let x b e) = do res <- dsem' b
                       modify $ M.insert x res
                       dsem' e
dsem' (Ref x) = do st <- get
                   lift $ M.lookup x st
dsem' (Fun x e)   = return $ DF x e
dsem' (App l r)   = do st <- get
                       DF x e <- dsem' l
                       v <- dsem' r
                       modify $ M.insert x v
                       dsem' e

exSucc :: Exp
exSucc = Let "succ" (Fun "x" (Add (Ref "x") (Lit 1)))
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

exSucc2 :: Exp
exSucc2 = Let "succ" (Lit 2)
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

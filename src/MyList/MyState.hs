module MyList.MyState where

import Control.Monad.State
import Control.Monad (liftM2)
import qualified Data.Map as M

--
-- * Abstract syntax
--


-- * Lets say we have some simple language with variable binding

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

-- | Now when we right a sem function how will we handle the store for
-- variables?

-- We could do it this way:
type Env = M.Map Var

-- and now we would just write sem like this mapping variables to DVals:
-- | Semantic function.
dsem :: Exp -> Env DVal -> Maybe DVal
dsem (Lit i)     _ = Just (DI i)
dsem (Add l r)   m = case (dsem l m, dsem r m) of
                       (Just (DI i), Just (DI j)) -> Just (DI (i+j))
                       _ -> Nothing
dsem (Let x b e) m = case dsem b m of
                       Just v -> dsem e (M.insert x v m)
                       _ -> Nothing
dsem (Ref x)     m = M.lookup x m
dsem (Fun x e)   _ = Just (DF x e)
dsem (App l r)   m = case (dsem l m, dsem r m) of
                       (Just (DF x e), Just v) -> dsem e (M.insert x v m)
                       _ -> Nothing


-- | But notice the computation dsem is performing is dependent on the Env so we
-- need it, but it is really auxillary information, we are just carrying it
-- around with us and us it when we need and don't when we don't

-- Lets rewrite this with a state monad like so
type Env' = StateT (Env DVal) Maybe

-- and the un-eta reduced version, where a is the return type
-- type Env' a = StateT (Env DVal) Maybe a

-- Now we can rewrite our sem function like so:
dsem' :: Exp -> Env' DVal
dsem' (Lit i)     = return $ (DI i)
dsem' (Add l r)   = do (DI l') <- dsem' l
                       (DI r') <- dsem' r
                       return . DI $ l' + r'
                       -- liftM2 (+) (dsem' l) (dsem' r)
dsem' (Let x b e) = do st <- get
                       res <- dsem' b
                       -- modify $ M.insert x res
                       put $ M.insert x res st
                       dsem' e
dsem' (Ref x)     = do st <- get
                       lift $ M.lookup x st
dsem' (Fun x e)   = return $ (DF x e)
dsem' (App l r)   = do st <- get
                       (DF x e) <- dsem' l
                       v <- dsem' r
                       modify $ M.insert x v
                       dsem' e


exSucc :: Exp
exSucc = Let "succ" (Fun "x" (Add (Ref "x") (Lit 1)))
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

exSucc2 :: Exp
exSucc2 = Let "succ" (Lit 2)
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

-- runStateT (dsem' exSucc) M.empty

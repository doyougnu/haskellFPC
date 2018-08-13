module MyLang where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad (liftM2)
import Data.String (IsString)
import qualified Data.Map as M

-- * Last time we had a toy language like this:

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

-- | The environment to store variables in
type Env = M.Map Var

-- * And we created our semantic function using a monad transformer stack like
-- * this

type Env' = StateT (Env DVal) Maybe

sem :: Exp -> Env' DVal
sem (Lit i)     = return $ (DI i)
sem (Add l r)   = do (DI l') <- sem l
                     (DI r') <- sem r
                     return . DI $ l' + r'
sem (Let x b e) = do st <- get
                     res <- sem b
                     put $ M.insert x res st
                     sem e
sem (Ref x)     = do st <- get
                     lift $ M.lookup x st
sem (Fun x e)   = return $ (DF x e)
sem (App l r)   = do st <- get
                     (DF x e) <- sem l
                     v <- sem r
                     modify $ M.insert x v
                     sem e

-- * which was a huge improvement over our non-monadic function. But let's get
-- * more crazy and add a Writer and Reader monad to our stack now

newtype Ctx = Ctx {logging :: Bool}

type BigEnv = StateT (Env DVal) (ReaderT Ctx (WriterT String Maybe))

-- | Let's make our own tell functions
debug :: (IsString a, MonadWriter a m) => a -> m ()
debug = tell . (<>) "\n[Debug]: "

result :: String -> BigEnv ()
result = tell . (++) "\n[Result]: "

-- | we can refactor out our reader and writer interactions
logWhen :: MonadReader Ctx m => m () -> m ()
logWhen f = do shouldLog <- ask
               if (logging shouldLog)
                 then f
                 else return ()

-- sem' :: Exp -> BigEnv DVal
sem' :: Exp -> BigEnv DVal
sem' (Lit i)     = return $ (DI i)
sem' (Add l r)   = do shouldLog <- ask
                      x@(DI l') <- sem' l
                      y@(DI r') <- sem' r

                      -- logWhen (do debug $ show x ++ " + " ++ show y
                      --             result . show $ l' + r')

                      if (logging shouldLog)
                        then do debug $ show x ++ " + " ++ show y
                                result . show $ l' + r'
                        else return ()

                      return . DI $ l' + r'
sem' (Let x b e) = do shouldLog <- ask
                      st <- get
                      res <- sem' b

                      -- logWhen (debug $ "Letting: " ++ show x ++ " = " ++ show b
                      --        ++ " in " ++ show e)

                      if (logging shouldLog)
                        then debug $ "Letting: "
                             <> show x
                             <> " = "
                             <> show b
                             <> " in "
                             <> show e
                        else return ()

                      put $ M.insert x res st
                      sem' e
sem' (Ref x)     = do st <- get
                      shouldLog <- ask
                      if (logging shouldLog)
                        then debug $ "Got reference " ++ show x
                        else return ()
                      lift . lift . lift $ M.lookup x st
sem' (Fun x e)   = do shouldLog <- ask
                      if (logging shouldLog)
                        then debug $ "returning function: "
                             <> show x
                             <> " with body: "
                             <> show e
                        else return ()

                      return $ (DF x e)
sem' (App l r)   = do st <- get
                      (DF x e) <- sem' l

                      shouldLog <- ask
                      if (logging shouldLog)
                        then debug $ "applying: "
                             <> show l
                             <> " to "
                             <> show r
                        else return ()

                      v <- sem' r
                      modify $ M.insert x v
                      sem' e

sem'' :: Exp -> BigEnv DVal
sem'' (Lit i)     = return $ (DI i)
sem'' (Add l r)   = do shouldLog <- ask
                       x@(DI l') <- sem'' l
                       y@(DI r') <- sem'' r

                       logWhen (do debug $ show x ++ " + " ++ show y
                                   result . show $ l' + r')

                       return . DI $ l' + r'
sem'' (Let x b e) = do shouldLog <- ask
                       st <- get
                       res <- sem'' b

                       logWhen (debug $ "Letting: "
                                <> show x
                                <> " = "
                                <> show b
                                <> " in "
                                <> show e)

                       put $ M.insert x res st
                       sem'' e
sem'' (Ref x)     = do st <- get
                       shouldLog <- ask
                       if (logging shouldLog)
                         then debug $ "Got reference " ++ show x
                         else return ()
                       lift . lift . lift $ M.lookup x st
sem'' (Fun x e)   = do shouldLog <- ask
                       if (logging shouldLog)
                         then debug $ "returning function: "
                              <> show x
                              <> " with body: "
                              <> show e
                         else return ()

                       return $ (DF x e)
sem'' (App l r)   = do st <- get
                       (DF x e) <- sem'' l

                       shouldLog <- ask
                       if (logging shouldLog)
                         then debug $ "applying: "
                              <> show l
                              <> " to "
                              <> show r
                         else return ()

                       v <- sem'' r
                       modify $ M.insert x v
                       sem'' e


exSucc :: Exp
exSucc = Let "succ" (Fun "x" (Add (Ref "x") (Lit 1)))
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

exSucc2 :: Exp
exSucc2 = Let "succ" (Lit 2)
             (App (Ref "succ") (App (Ref "succ") (Lit 5)))

-- | run with
-- runWriterT (runReaderT (runStateT (sem' exSucc) M.empty) (Ctx True))

runBigEnv :: Exp -> Maybe ((DVal, Env DVal), String)
runBigEnv x = runWriterT (runReaderT (runStateT (sem' x) M.empty) (Ctx True))

getState = fmap fst' . runBigEnv
  where fst' ((a,_),_) = a

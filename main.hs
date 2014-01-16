module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Eval a = ReaderT Env (ErrorT String Identity) a
type Name   = String
data Exp    = Lit Integer
            | Var Name
            | Plus Exp Exp
            | Abs Name Exp
            | App Exp Exp
            deriving (Show)
data Value  = IntVal Integer
            | FunVal Env Name Exp
            deriving (Show)
type Env    = Map.Map Name Value 

runEval :: Env -> Eval a -> Either String a
runEval env ev = runIdentity $ runErrorT $ runReaderT ev env

eval :: Exp -> Eval Value
eval (Lit i) = return $ IntVal i
eval (Var n) = do env <- ask
                  case Map.lookup n env of
                      Nothing -> throwError $ "undefined value: " ++ n
                      Just val -> return val
eval (Plus e1 e2) = do v1 <- eval e1
                       v2 <- eval e2
                       case (v1, v2) of
                           (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                           _ -> throwError "type error in plus"
eval (Abs n e) = do env <- ask
                    return $ FunVal env n e
eval (App e1 e2) = do val1 <- eval e1
                      val2 <- eval e2
                      case val1 of
                          FunVal env' n body -> local (Map.insert n val2) (eval body)
                          _ -> throwError "type error in application"


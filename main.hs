module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

type Eval a = ErrorT String Identity a
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

runEval :: Eval a -> Either String a
runEval = runIdentity . runErrorT

eval :: Env -> Exp -> Eval Value
eval env (Lit i) = return $ IntVal i
eval env (Var n) = case Map.lookup n env of
                       Nothing -> throwError $ "undefined value: " ++ n
                       Just val -> return val
eval env (Plus e1 e2) = do v1 <- eval env e1
                           v2 <- eval env e2
                           case (v1, v2) of
                               (IntVal i1, IntVal i2) -> return $ IntVal $ i1 + i2
                               _ -> throwError "type error in plus"
eval env (Abs n e) = return $ FunVal env n e
eval env (App e1 e2) = do val1 <- eval env e1
                          val2 <- eval env e2
                          case val1 of
                              FunVal env' n body -> eval (Map.insert n val2 env') body
                              _ -> throwError "type error in application"


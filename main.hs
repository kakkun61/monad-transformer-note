module Transformers where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

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

eval :: Env -> Exp -> Value
eval env (Lit i) = IntVal i
eval env (Var n) = fromJust $ Map.lookup n env
eval env (Plus e1 e2) = let IntVal i1 = eval env e1
                            IntVal i2 = eval env e2
                        in IntVal (i1 + i2)
eval env (Abs n e) = FunVal env n e
eval env (App e1 e2) = let val1 = eval env e1
                           val2 = eval env e2
                       in case val1 of
                           FunVal env' n body -> eval (Map.insert n val2 env') body


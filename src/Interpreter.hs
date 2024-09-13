module Interpreter where

import AST

import Data.HashMap.Strict
import Data.List (elemIndex)
import Control.Monad.State

type Env = HashMap Ident Indexed
type InterpretT = StateT Env

type IndexST = StateT [Ident]

runInterpretT :: Monad m => InterpretT m a -> m a
runInterpretT = flip evalStateT empty


-- Gives De Brujin indices to each variable
index :: Monad m => Expr -> IndexST m Indexed
index (Apply expr1 expr2) = Apply <$> index expr1 <*> index expr2
index (Lambda var expr) = Lambda var <$> (modify (var :) *> index expr <* modify tail)
index (Var var) = do
    maybeVar <- gets $ fmap Var . elemIndex var
    case maybeVar of
        Just v -> return v
        Nothing -> do
            i <- gets length
            modify (++ [var])
            return $ Var i

-- Recovers original variable names
deindex :: Monad m => Indexed -> IndexST m Expr
deindex (Var i) = gets $ Var . (!! i)
deindex (Apply expr1 expr2) = Apply <$> deindex expr1 <*> deindex expr2
deindex (Lambda var expr) = Lambda var <$> (modify (var :) *> deindex expr <* modify tail)

-- Helper to increment variable indices
incVar :: Indexed -> Indexed
incVar (Var i) = Var $ i + 1
incVar (Apply expr1 expr2) = Apply (incVar expr1) (incVar expr2)
incVar (Lambda var expr) = Lambda var $ incVar expr

-- Replaces variables with an expression
subst :: Int -> Indexed -> Indexed -> Indexed
subst i expr1@(Var var) expr2 = if i == var then expr2 else expr1
subst i (Apply expr1 expr2) expr = Apply (subst i expr1 expr) (subst i expr2 expr)
subst i (Lambda var expr1) expr2 = Lambda var $ subst (i + 1) expr1 (incVar expr2)

-- Evaluates an indexed expression
eval :: Monad m => Indexed -> InterpretT m Indexed
eval (Apply (Lambda _ expr1) expr2) = eval $ subst 0 expr1 expr2
eval (Apply expr1 expr2) = Apply <$> eval expr1 <*> eval expr2
eval (Lambda var expr) = Lambda var <$> eval expr
eval x = return x

-- Evaluates a normal expression
interpret :: Monad m => Expr -> InterpretT m Expr
interpret = flip evalStateT [] . action
    where
        action :: Monad m => Expr -> IndexST (InterpretT m) Expr
        action = deindex <=< lift . eval <=< index

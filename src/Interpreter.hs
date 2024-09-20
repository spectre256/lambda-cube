module Interpreter where

import AST

import Data.HashMap.Strict
import Data.List (elemIndex)
import Control.Monad.State
import Control.Monad.Identity

type Env = HashMap Ident Indexed
type InterpretT = StateT Env
type Interpret = InterpretT Identity

type IndexST = StateT [Ident]

runInterpretT :: Monad m => InterpretT m a -> m a
runInterpretT = flip evalStateT empty

runInterpret :: Interpret a -> a
runInterpret = runIdentity . runInterpretT


-- Gives De Brujin indices to each variable
index :: Monad m => Expr -> IndexST m Indexed
index (Apply expr1 expr2) = Apply <$> index expr1 <*> index expr2
index (Lambda var expr) = Lambda var <$> withVar var (index expr)
index (Var var) = do
    maybeVar <- gets $ elemIndex var
    case maybeVar of
        Just v -> return $ Var v
        Nothing -> do
            i <- gets length
            modify (++ [var])
            return $ Var i

-- Recovers original variable names
deindex :: Monad m => Indexed -> IndexST m Expr
deindex (Var i) = gets $ Var . (!! i)
deindex (Apply expr1 expr2) = Apply <$> deindex expr1 <*> deindex expr2
deindex (Lambda var expr) = Lambda var <$> withVar var (deindex expr)

withVar :: Monad m => Ident -> IndexST m a -> IndexST m a
withVar var action = modify (var :) *> action <* modify tail

-- Helper to increment variable indices
mapVar :: (Int -> Int) -> Indexed -> Indexed
mapVar f (Var i) = Var $ f i
mapVar f (Apply expr1 expr2) = Apply (mapVar f expr1) (mapVar f expr2)
mapVar f (Lambda var expr) = Lambda var $ mapVar f expr

-- Replaces variable i with an expression and updates variable indices accordingly
subst :: Int -> Indexed -> Indexed -> Indexed
subst i (Var i') expr
    | i' == i = expr
    | i' > i = Var $ i' - 1
    | otherwise = Var i'
subst i (Apply expr1 expr2) expr = Apply (subst i expr1 expr) (subst i expr2 expr)
subst i (Lambda var expr1) expr2 = Lambda var $ subst (i + 1) expr1 (mapVar (+ 1) expr2)

-- Applies a lambda abstraction
apply :: Indexed -> Indexed -> Indexed
apply (Lambda _ expr1) expr2 = subst 0 expr1 expr2
apply x y = Apply x y

-- Evaluates an indexed expression
eval :: Monad m => Indexed -> InterpretT m Indexed
eval (Apply expr1 expr2) = apply <$> eval expr1 <*> eval expr2
eval (Lambda var expr) = Lambda var <$> eval expr
eval x = return x

-- Evaluates a normal expression
interpret :: Monad m => Expr -> InterpretT m Expr
interpret = flip evalStateT [] . action
    where action = deindex <=< lift . eval <=< index

interpret' :: Monad m => Expr -> InterpretT m Indexed
interpret' = flip evalStateT [] . action
    where action = lift . eval <=< index

module Interpreter where

import AST

import Control.Monad.State
import Data.List (elemIndex)


-- Gives De Brujin indices to each variable
index :: Expr -> State [Ident] Indexed
index (Var var) = do
    maybeVar <- gets $ fmap Var . elemIndex var
    case maybeVar of
        Just v -> return v
        Nothing -> do
            i <- gets length
            modify (++ [var])
            return $ Var i
index (Apply expr1 expr2) = Apply <$> index expr1 <*> index expr2
index (Lambda var expr) = Lambda var <$> (modify (var :) *> index expr <* modify tail)

-- Recover original variable names
deindex :: Indexed -> State [Ident] Expr
deindex (Var i) = gets $ Var . (!! i)
deindex (Apply expr1 expr2) = Apply <$> deindex expr1 <*> deindex expr2
deindex (Lambda var expr) = Lambda var <$> (modify (var :) *> deindex expr <* modify tail)

-- Replaces variables with an expression
subst :: Int -> Indexed -> Indexed -> Indexed
subst i expr1@(Var var) expr2 = if i == var then expr2 else expr1
subst i (Apply expr1 expr2) expr = Apply (subst i expr1 expr) (subst i expr2 expr)
subst i (Lambda var expr1) expr2 = Lambda var $ subst (i + 1) expr1 expr2

-- Evaluates an expression
eval :: Indexed -> Indexed
eval (Apply (Lambda _ expr1) expr2) = eval $ subst 0 expr1 expr2
eval (Apply expr1 expr2) = Apply (eval expr1) (eval expr2)
eval (Lambda var expr) = Lambda var $ eval expr
eval x = x

interpret :: Expr -> Expr
interpret = flip evalState [] . (deindex . eval <=< index)

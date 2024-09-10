module Interpreter where

import AST

import Data.Text (Text)
import Data.HashSet
import Control.Monad.State
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- Gives De Brujin indices to each variable
index :: Expr -> Indexed
index = index' []
    where
        -- Does not handle free variables
        index' :: [Ident] -> Expr -> Indexed
        index' vars (Var var) = Var . fromMaybe (-1 :: Int) $ elemIndex var vars
        index' vars (Apply expr1 expr2) = Apply (index' vars expr1) (index' vars expr2)
        index' vars (Lambda var expr) = Lambda var $ index' (var : vars) expr

        -- Gives indices to free variables
        fixup :: Indexed -> State Int Indexed
        fixup = undefined

-- Recover original variable names. TODO: Figure out how free variables are supposed to work here
deindex :: Indexed -> Expr
deindex = undefined


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

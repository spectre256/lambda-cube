{-# LANGUAGE OverloadedStrings #-}

module Interpreter where

import AST

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (elemIndex)
import Control.Monad.State
import Control.Monad.Identity

type Env = HashMap Ident Indexed
type InterpretT = StateT Env
type Interpret = InterpretT Identity

type IndexST = StateT [Ident]
type Index = IndexST Identity

runInterpretT :: Monad m => InterpretT m a -> m a
runInterpretT = flip evalStateT H.empty

runInterpret :: Interpret a -> a
runInterpret = runIdentity . runInterpretT

runIndexST :: Monad m => IndexST m a -> m a
runIndexST = flip evalStateT []

runIndex :: Index a -> a
runIndex = runIdentity . runIndexST

-- Gives De Brujin indices to each variable
index :: Monad m => Expr -> IndexST m Indexed
index (Apply expr1 expr2) = Apply <$> index expr1 <*> index expr2
index (Lambda var expr) = Lambda var <$> withVar var (index expr)
index (Ref ref) = return $ Ref ref
index (Var var) = gets $ maybe (Ref var) Var . elemIndex var

-- Recovers original variable names
deindex :: Monad m => Indexed -> IndexST m Expr
deindex (Var i) = gets $ Var . (!! i)
deindex (Ref ref) = return $ Ref ref -- Yes, it has to be rewrapped in a Ref to avoid a type error
deindex (Apply expr1 expr2) = Apply <$> deindex expr1 <*> deindex expr2
deindex (Lambda var expr) = Lambda var <$> withVar var (deindex expr)

withVar :: Monad m => Ident -> IndexST m a -> IndexST m a
withVar var action = modify (var :) *> action <* modify tail

-- Helper to increment variable indices
mapVar :: (Int -> Int) -> Indexed -> Indexed
mapVar f (Var i) = Var $ f i
mapVar f (Apply expr1 expr2) = Apply (mapVar f expr1) (mapVar f expr2)
mapVar f (Lambda var expr) = Lambda var $ mapVar f expr
mapVar _ x = x

expand :: Monad m => Indexed -> InterpretT m Indexed
expand (Ref ref) = gets (H.! ref)
expand (Lambda var expr) = Lambda var <$> expand expr
expand (Apply expr1 expr2) = Apply <$> expand expr1 <*> expand expr2
expand x = return x

-- Replaces variable i with an expression and updates variable indices accordingly
subst :: Int -> Indexed -> Indexed -> Indexed
subst i (Var i') expr
    | i' == i = expr
    | i' > i = Var $ i' - 1
    | otherwise = Var i'
subst i (Apply expr1 expr2) expr = Apply (subst i expr1 expr) (subst i expr2 expr)
subst i (Lambda var expr1) expr2 = Lambda var $ subst (i + 1) expr1 (mapVar (+ 1) expr2)
subst _ x _ = x

-- Applies a lambda abstraction
apply :: Monad m => Indexed -> Indexed -> InterpretT m Indexed
apply (Lambda _ expr1) expr2 = eval $ subst 0 expr1 expr2
apply x y = return $ Apply x y

-- Helper for the apply command
apply1 :: Indexed -> Indexed
apply1 (Apply (Lambda _ expr1) expr2) = subst 0 expr1 expr2
apply1 x = x

-- Evaluates an indexed expression
eval :: Monad m => Indexed -> InterpretT m Indexed
eval (Apply expr1 expr2) = do
    res1 <- eval expr1
    res2 <- eval expr2
    apply res1 res2
eval (Lambda var expr) = Lambda var <$> eval expr
eval (Ref ref) = eval =<< gets (H.! ref) -- TODO: Error handling
eval x = return x

-- Evaluates a normal expression
interpret :: Monad m => Expr -> InterpretT m Expr
interpret = runIndexST . (deindex <=< lift . eval <=< index)

interpret' :: Monad m => Expr -> InterpretT m Indexed
interpret' = runIndexST . (lift . eval <=< index)

-- Executes a statement (a binding or an expression to evaluate)
exec :: Monad m => Stmt -> InterpretT m String
exec (DeclVar var expr) = do
    let indexed = runIndex $ index expr
    modify (H.insert var indexed)
    return $ pretty expr
exec (EvalExpr expr) = pretty <$> interpret expr
exec (Cmd ident expr) = case ident of
    "parse" -> return $ show expr
    "index" -> return . pretty . runIndex $ index expr
    "index'" -> return . show . runIndex $ index expr
    "expand" -> pretty <$> runExpand expr
        where runExpand = runIndexST . (deindex <=< lift . expand <=< index)
    "apply" -> pretty <$> runApply expr
        where runApply = runIndexST . (deindex . apply1 <=< lift . expand <=< index)
    _ -> return "Invalid command"


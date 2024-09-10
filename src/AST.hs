module AST where

import Data.Text (Text)

type Ident = Text

type Expr = Expr' Ident Ident
type Indexed = Expr' Int Ident

data Expr' a b
    = Var a
    | Apply (Expr' a b) (Expr' a b)
    | Lambda b (Expr' a b)
    deriving (Show, Eq)

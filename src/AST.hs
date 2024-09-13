module AST where

import Data.Text (Text, unpack)

type Ident = Text

type Expr = Expr' Ident Ident
type Indexed = Expr' Int Ident

data Expr' a b
    = Var a
    | Apply (Expr' a b) (Expr' a b)
    | Lambda b (Expr' a b)
    deriving (Eq)


showParens :: Show a => a -> String
showParens x = '(' : show x ++ ")"

showTermE :: Expr -> String
showTermE e@(Var _) = show e
showTermE e = showParens e

showTermI :: Indexed -> String
showTermI e@(Var _) = show e
showTermI e = showParens e

instance Show Expr where
    show (Var var) = unpack var
    show (Apply expr1 expr2) = showTermE expr1 ++ " " ++ showTermE expr2
    show (Lambda var expr) = "λ" ++ unpack var ++ ". " ++ show expr

instance Show Indexed where
    show (Var i) = show i
    show (Apply expr1 expr2) = showTermI expr1 ++ " " ++ showTermI expr2
    show (Lambda _ expr) = "λ. " ++ show expr

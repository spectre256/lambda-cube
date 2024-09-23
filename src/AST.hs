module AST where

import Data.Text (Text, unpack)

type Ident = Text

type Expr = Expr' Ident Ident
type Indexed = Expr' Int Ident

data Expr' a b
    = Var a
    | Ref Ident
    | Apply (Expr' a b) (Expr' a b)
    | Lambda b (Expr' a b)
    deriving (Eq)

data Stmt
    = DeclVar Ident Expr
    | EvalExpr Expr
    deriving (Show, Eq)


showParens :: Show a => a -> String
showParens x = '(' : show x ++ ")"

showTerm :: Show (Expr' a b) => Expr' a b -> String
showTerm e@(Var _) = show e
showTerm e = showParens e

instance Show Expr where
    show (Var var) = unpack var
    show (Ref ref) = unpack ref
    show (Apply expr1 expr2) = showTerm expr1 ++ " " ++ showTerm expr2
    show (Lambda var expr) = "λ" ++ unpack var ++ ". " ++ show expr

instance Show Indexed where
    show (Var i) = show i
    show (Ref ref) = unpack ref
    show (Apply expr1 expr2) = showTerm expr1 ++ " " ++ showTerm expr2
    show (Lambda _ expr) = "λ. " ++ show expr

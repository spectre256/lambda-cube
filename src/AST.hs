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
    deriving (Show, Eq)

data Stmt
    = DeclVar Ident Expr
    | EvalExpr Expr
    | Cmd Ident Expr
    deriving (Show, Eq)


class Pretty a where
    pretty :: a -> String

prettyParens :: Pretty a => a -> String
prettyParens x = '(' : pretty x ++ ")"

prettyTerm :: Pretty (Expr' a b) => Expr' a b -> String
prettyTerm e@(Var _) = pretty e
prettyTerm e = prettyParens e

instance Pretty Expr where
    pretty (Var var) = unpack var
    pretty (Ref ref) = unpack ref
    pretty (Apply expr1 expr2) = pretty expr1 ++ " " ++ prettyTerm expr2
    pretty (Lambda var expr) = "λ" ++ unpack var ++ ". " ++ pretty expr

instance Pretty Indexed where
    pretty (Var i) = show i
    pretty (Ref ref) = unpack ref
    pretty (Apply expr1 expr2) = prettyTerm expr1 ++ " " ++ prettyTerm expr2
    pretty (Lambda _ expr) = "λ. " ++ pretty expr

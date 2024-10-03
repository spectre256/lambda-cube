module AST where

import Data.Text (Text, unpack)
import Data.Scientific (Scientific)

type Ident = Text

type Expr = Expr' Ident Ident
type Indexed = Expr' Int Ident

data Literal
    = NumLit Scientific
    | BoolLit Bool
    | CharLit Char
    | StrLit Text
    deriving (Show, Eq)

data Expr' a b
    = Lit Literal
    | Var a
    | Ref Ident
    | Apply (Expr' a b) (Expr' a b)
    | Lambda b Ty (Expr' a b)
    deriving (Show, Eq)

data Stmt
    = DeclVar Ident Expr
    | EvalExpr Expr
    | Cmd Ident Expr
    deriving (Show, Eq)

data Ty
    = NumTy
    | BoolTy
    | CharTy
    | StrTy
    | FnTy Ty Ty
    deriving (Show, Eq)


class Pretty a where
    pretty :: a -> String


instance Pretty Literal where
    pretty (NumLit n) = show n
    pretty (BoolLit b) = if b then "true" else "false"
    pretty (CharLit c) = show c
    pretty (StrLit s) = unpack s

prettyParens :: Pretty a => a -> String
prettyParens x = '(' : pretty x ++ ")"

prettyTerm :: Pretty (Expr' a b) => Expr' a b -> String
prettyTerm e@(Var _) = pretty e
prettyTerm e = prettyParens e

prettyApplyTerm :: Pretty (Expr' a b) => Expr' a b -> String
prettyApplyTerm e@(Var _) = pretty e
prettyApplyTerm e@(Apply _ _) = pretty e
prettyApplyTerm e = prettyParens e

instance Pretty Expr where
    pretty (Lit lit) = pretty lit
    pretty (Var var) = unpack var
    pretty (Ref ref) = unpack ref
    pretty (Apply expr1 expr2) = prettyApplyTerm expr1 ++ " " ++ prettyTerm expr2
    pretty (Lambda var ty expr) = "λ" ++ unpack var ++ " : " ++ pretty ty ++ ". " ++ pretty expr

instance Pretty Indexed where
    pretty (Lit lit) = pretty lit
    pretty (Var i) = show i
    pretty (Ref ref) = unpack ref
    pretty (Apply expr1 expr2) = prettyTerm expr1 ++ " " ++ prettyTerm expr2
    pretty (Lambda _ _ expr) = "λ. " ++ pretty expr


prettyTyTerm :: Ty -> String
prettyTyTerm ty@(FnTy _ _) = prettyParens ty
prettyTyTerm ty = pretty ty

instance Pretty Ty where
    pretty NumTy = "Num"
    pretty BoolTy = "Bool"
    pretty CharTy = "Char"
    pretty StrTy = "Str"
    pretty (FnTy ty1 ty2) = prettyTyTerm ty1 ++ " -> " ++ pretty ty2

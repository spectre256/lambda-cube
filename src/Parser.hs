{-# LANGUAGE OverloadedStrings #-}

module Parser where

import AST

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (foldl')

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme hspace

symbol :: Text -> Parser Text
symbol = L.symbol hspace

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseIdent :: Parser Text
parseIdent = lexeme $ T.cons <$> letterChar <*> takeWhileP Nothing isAlphaNum

parseVar :: Parser Expr
parseVar = Var <$> parseIdent

parseApply :: Parser Expr
parseApply = foldl' Apply <$> parseExprTerm <*> some parseExprTerm

parseLambda :: Parser Expr
parseLambda = Lambda <$> (lambda *> parseIdent <* symbol ".") <*> parseExpr
    where lambda = symbol "\\" <|> symbol "λ"

parseExprTerm :: Parser Expr
parseExprTerm = parseVar <|> parens parseExpr

parseExpr :: Parser Expr
parseExpr = choice
    [ try parseApply
    , parseLambda
    , parseExprTerm ]

parseDecl :: Parser Stmt
parseDecl = DeclVar <$> parseIdent <* symbol "=" <*> parseExpr

parseCmd :: Parser Stmt
parseCmd = Cmd <$> (symbol ":" *> parseIdent) <*> parseExpr

parseStmt :: Parser Stmt
parseStmt = hspace *> choice
    [ try parseDecl
    , parseCmd
    , EvalExpr <$> parseExpr ]

parse :: Text -> Maybe Stmt
parse = parseMaybe parseStmt

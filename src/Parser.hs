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
parseApply = Apply <$> parseExprTerm <*> parseExprTerm

parseLambda :: Parser Expr
parseLambda = Lambda <$> (symbol "\\" *> parseIdent <* symbol ".") <*> parseExpr

parseExprTerm :: Parser Expr
parseExprTerm = choice
    [ parseVar
    , parseLambda
    , parens parseExpr ]

parseExpr :: Parser Expr
parseExpr = try parseApply <|> parseExprTerm

parse :: Text -> Maybe Expr
parse = parseMaybe parseExpr

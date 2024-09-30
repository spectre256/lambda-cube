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

parseLit :: Parser Literal
parseLit = choice
    [ NumLit <$> parseNum
    , BoolLit <$> parseBool
    , CharLit <$> parseChar
    , StrLit . T.pack <$> parseString ]
    where
        parseNum = lexeme $ L.signed (pure ()) L.scientific
        parseBool = True <$ symbol "true" <|> False <$ symbol "false"
        parseChar = char '\'' *> L.charLiteral <* symbol "'"
        parseString = char '"' *> manyTill L.charLiteral (char '"')

parseVar :: Parser Expr
parseVar = Var <$> parseIdent

parseApply :: Parser Expr
parseApply = foldl' Apply <$> parseExprTerm <*> some parseExprTerm

parseLambda :: Parser Expr
parseLambda = do
    vars <- let lambda = symbol "\\" <|> symbol "λ"
        in lambda *> some parseIdent <* symbol "."
    expr <- parseExpr
    return $ foldr Lambda expr vars

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

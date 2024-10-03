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
import Data.List (foldl', singleton)

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

parseApply :: Parser Expr
parseApply = foldl' Apply <$> parseExprTerm <*> some parseExprTerm

parseLambda :: Parser Expr
parseLambda = do
    varPairs <- lambda *> vars <* symbol "."
    expr <- parseExpr
    return $ foldr (uncurry Lambda) expr varPairs
    where
        lambda = symbol "\\" <|> symbol "λ"
        var = (,) <$> parseIdent <* symbol ":" <*> parseTy
        vars = some (parens var) <|> fmap singleton var

parseExprTerm :: Parser Expr
parseExprTerm = choice
    [ Lit <$> parseLit
    , Var <$> parseIdent
    , parens parseExpr ]

parseExpr :: Parser Expr
parseExpr = choice
    [ try parseApply
    , parseLambda
    , parseExprTerm ]

parseTyTerm :: Parser Ty
parseTyTerm = choice
    [ NumTy <$ symbol "Num"
    , BoolTy <$ symbol "Bool"
    , CharTy <$ symbol "Char"
    , StrTy <$ symbol "Str" ]

parseTy :: Parser Ty
parseTy = choice
    [ try $ FnTy <$> parseTyTerm <* symbol "->" <*> parseTyTerm
    , parens parseTy
    , parseTyTerm ]

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

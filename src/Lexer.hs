module Lexer where

import Text.Parsec

data Token = TName String | TVar String | TSym String deriving Show

type Lexer = Parsec String ()

nameL :: Lexer Token
nameL = TName <$> (lowerName <|> quoted)
  where
    lowerName = pure (:) <*> lower <*> (many (alphaNum <|> char '_'))
    quoted = char '\'' *> many alphaNum <* char '\''

varL :: Lexer Token
varL = TVar <$> (upperName <|> string "_")
  where
    upperName = pure (:) <*> upper <*> (many (alphaNum <|> char '_'))

symL :: Lexer Token
symL = TSym <$> (string ":-" <|> (choice . map (string . (: [])) $ "[]|().;,"))

tokensL :: Lexer [(SourcePos, Token)]
tokensL = between spaces spaces . flip sepBy spaces $ (pure (,) <*> getPosition <*> tokenP)
  where
    tokenP = nameL <|> varL <|> symL
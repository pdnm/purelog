module Parser where

import Text.Parsec
import Lexer
import AST

type Parser = Parsec [(SourcePos, Token)] ()

tokenP :: (Token -> Maybe a) -> Parser a
tokenP test = token show fst (test . snd)

symbol :: String -> Parser ()
symbol c = tokenP (\t -> case t of
  TSym s -> if s == c then Just () else Nothing
  _ -> Nothing)

functorP :: Parser (String, [Term]) -- functor and relation have the same parser
functorP = do
  name <- tokenP (\t -> case t of
                      (TName s) -> Just s
                      _ -> Nothing)
  terms <- between (symbol "(") (symbol ")")
          . flip sepBy1 (symbol ",") $ termP
  return (name, terms)

termP :: Parser Term
termP = do
    name <- tokenP (\t -> case t of
                        (TName s) -> Just (Atom s)
                        (TVar s) -> Just (Var s)
                        _ -> Nothing)
    case name of -- parser consumes name which can be of atom or functor
      (Atom a) -> (fmap (Func a) . between (symbol "(") (symbol ")")
                 . flip sepBy1 (symbol ",") $ termP) <|> return name
      _ -> return name

relP :: Parser Rel
relP = fmap (uncurry Rel) functorP

ruleP :: Parser Rule
ruleP = do
  head <- relP
  body <-
    (symbol "." *> return [[]]) <|>
    (symbol ":-" *>
        (flip sepBy (symbol ";") . flip sepBy (symbol ",") $ relP) <*
      symbol ".")
  return $ Rule head body

programP :: Parser Program
programP = fmap Program $ many ruleP

parseProgram :: String -> Either ParseError Program
parseProgram source = do
  tokens  <- parse (tokensL   <* eof) "" source
  parse (programP <* eof) "" tokens

parseRel :: String -> Either ParseError Rel
parseRel source = do
  tokens  <- parse (tokensL   <* eof) "" source
  parse (relP <* (symbol ".") <* eof) "" tokens
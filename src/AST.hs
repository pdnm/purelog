module AST where

import qualified Data.List as L

newtype Program = Program [Rule]

data Rule = Rule Rel [[Rel]]

data Rel = Rel String [Term]

data Term =
  Atom String
  | Var String
  | Func String [Term] deriving (Eq, Ord)

instance Show Term where
  show (Atom a) = a
  show (Var x) = x
  show (Func name terms) = name ++ "(" ++ L.intercalate ", " (map show terms) ++ ")"

module AST where

import qualified Data.List as L

newtype Program = Program [Rule]

data Rule = Rule Rel [[Rel]]

data Rel = Rel String [Term]

data Term =
  Atom String
  | Var String
  | Func String [Term] deriving (Eq, Ord)

toList :: Term -> [Term]
toList (Atom ".") = []
toList (Func "." [hd, tl]) = hd : toList tl
toList term = [term]

instance Show Term where
  show (Atom ".") = "[]"
  show (Atom a) = a
  show (Var x) = x
  show func@(Func name terms) = case name of
    "." -> show . toList $ func
    _ -> name ++ "(" ++ L.intercalate ", " (map show terms) ++ ")"

module Engine where

import AST
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Goal = Goal Table [Rel]
data Tree = Tree Goal [Tree] Bool -- isExpanded
type Table = M.Map String Term

initTree :: Rel -> Tree
initTree rel = Tree (Goal M.empty [rel]) [] False

search :: Program -> Tree -> Int -> Maybe (Table, Maybe Tree)
search program (Tree (Goal table []) _ _) height = Just (table, Nothing)

search program (Tree (Goal table ((Rel "equal" [t1, t2]) : tl)) [] False) height =
  case unify t1 t2 table of
    Just table' -> search program (Tree (Goal table' tl) [] False) height
    Nothing     -> Nothing

search program (Tree goal [] False) height =
  search program (Tree goal (expand program goal height) True) height

search program (Tree goal [] True) height = Nothing

search program (Tree goal (hd : tl) _) height =
  case search program hd (height + 1) of
    Just (table, Just hd')  -> Just (table, Just (Tree goal (hd' : tl) True))
    Just (table, Nothing)   -> Just (table, Just (Tree goal tl True))
    Nothing                 -> search program (Tree goal tl True) height


expand :: Program -> Goal -> Int -> [Tree]
expand (Program rules) (Goal table (hd : tl)) height = do
  rule <- filter (match hd) rules
  let (Rule pred body) = rename rule height
  disjunct <- body
  case unify (toFunctor pred) (toFunctor hd) table of
    Just table' -> return $ Tree (Goal table' (disjunct ++ tl)) [] False
    Nothing -> []


unify :: Term -> Term -> Table -> Maybe Table
unify (Atom a1) (Atom a2) table = if a1 == a2 then Just table else Nothing
unify (Func n1 t1) (Func n2 t2) table =
  if n1 == n2 && length t1 == length t2
    then foldl (>=>) Just (zipWith unify t1 t2) table
    else Nothing
unify (Var v) term table =
  case table M.!? v of
    Just term' -> unify term' term table
    Nothing -> Just (M.insert v term table)
unify term (var@(Var v)) table = unify var term table
unify _ _ _ = Nothing

rename :: Rule -> Int -> Rule
rename (Rule head body) height = Rule (renameRel head) (map (map renameRel) body)
  where
    renameRel = toRel . renameTerm . toFunctor
    toRel (Func name terms) = Rel name terms
    renameTerm (Var v)            = Var (v ++ show height)
    renameTerm (Func name terms)  = Func name (map renameTerm terms)
    renameTerm atom               = atom


toFunctor :: Rel -> Term
toFunctor (Rel name terms) = Func name terms

match :: Rel -> Rule -> Bool
match (Rel name terms) (Rule (Rel name' terms') _) =
  name == name' && length terms == length terms'

searchAll :: Program -> Tree -> [Table]
searchAll program tree =
  case search program tree 0 of
    Just (table, Just tree')  -> table : searchAll program tree'
    Just (table, Nothing)     -> [table]
    Nothing                   -> []

variables :: Rel -> [Term]
variables rel = S.toList . S.fromList . aux . toFunctor $ rel
  where
    aux v@(Var x) = [v]
    aux (Func _ terms) = terms >>= aux
    aux (Atom _) = []

resolve :: Table -> Term -> Term
resolve table (Var x) =
  case M.lookup x table of
    Just term -> resolve table term
    Nothing -> Var x

resolve table (Func name terms) = Func name (map (resolve table) terms)
resolve table a@(Atom _) = a
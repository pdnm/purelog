module Engine where

import AST
import Control.Monad
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L

data Goal = Goal Table [Rel]
data Tree = Tree Goal [Tree] Bool -- isExpanded
data Strategy = DFS | BFS

type Input = Rel
type Machine = (Program, Strategy)
type Table = M.Map String Term

newtype Solution = Solution [(String, Term)]

instance Show Solution where
  show (Solution vars) = L.intercalate ", " . map printVar $ vars
    where
      printVar (x, term) = x ++ " = " ++ show term


initTree :: Rel -> Tree
initTree rel = Tree (Goal M.empty [rel]) [] False

search :: Machine -> Input -> Tree -> Int -> Maybe (Solution, Maybe Tree)
search _ input (Tree (Goal table []) _ _) height =
  Just (getSolution input table, Nothing)

search machine input (Tree (Goal table ((Rel "equal" [t1, t2]) : tl)) [] False) height =
  case unify t1 t2 table of
    Just table' -> search machine input (Tree (Goal table' tl) [] False) height
    Nothing     -> Nothing

search machine input (Tree goal [] False) height =
  search machine input (Tree goal (expand machine goal height) True) height

search machine _ (Tree goal [] True) height = Nothing

search machine input (Tree goal (hd : tl) _) height =
  case search machine input hd (height + 1) of
    Just (table, Just hd')  -> Just (table, Just (Tree goal (hd' : tl) True))
    Just (table, Nothing)   ->
      case tl of
        []  -> Just (table, Nothing)
        _   -> Just (table, Just (Tree goal tl True))
    Nothing                 -> search machine input (Tree goal tl True) height


expand :: Machine -> Goal -> Int -> [Tree]
expand (Program rules, strat) (Goal table (hd : tl)) height = do
  rule <- filter (match hd) rules
  let (Rule pred body) = rename rule height
  disjunct <- body
  let newGoal rels newRels = case strat of
        DFS -> newRels ++ rels
        BFS -> rels ++ newRels
  case unify (toFunctor pred) (toFunctor hd) table of
    Just table' -> return $ Tree (Goal table' (newGoal tl disjunct)) [] False
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
unify term var@(Var v) table = unify var term table
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

searchAll :: Machine -> Input -> Tree -> [Solution]
searchAll machine input tree =
  case search machine input tree 0 of
    Just (solution, Just tree')  -> solution : searchAll machine input tree'
    Just (solution, Nothing)     -> [solution]
    Nothing                   -> []

variables :: Rel -> [Term]
variables = S.toList . S.fromList . aux . toFunctor
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

getSolution :: Rel -> Table -> Solution
getSolution rel table = Solution $ do
  (Var x) <- variables rel
  return (x, resolve table (Var x))

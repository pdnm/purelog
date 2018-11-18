module Main where

import System.Environment
import System.IO
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Parser
import AST
import Engine

data State = Idle | Search Rel Tree Int

showTruth :: Int -> IO ()
showTruth nSols = if nSols == 0 then putStrLn "false" else putStrLn "true"

printSolution :: Rel -> Table -> IO ()
printSolution query table = (putStr . L.intercalate ", " . map printVar $ (variables query)) >> hFlush stdout
  where
    printVar v@(Var x) = x ++ " = " ++ show (resolve table v)

findNext :: Program -> State -> IO ()
findNext program state@(Search rel tree found) =
  case search program tree 0 of
    Just (table, Just tree')  -> printSolution rel table >> run program (Search rel tree' (found + 1))
    Just (table, Nothing)     -> printSolution rel table >> putStrLn "." >> showTruth (found+1) >> run program Idle
    Nothing                   -> putStrLn "." >> showTruth found >> run program Idle

run :: Program -> State -> IO ()
run program state@(Search rel tree found) = if found == 0 then findNext program state else do
  command <- getLine
  case command of
    "."       -> showTruth found >> run program Idle
    ";"       -> findNext program state
    "#"       -> putStrLn ("Found " ++ show found ++ " solution(s).") >> run program state
    "$"       -> do
                  let tables = (searchAll program tree)
                  _ <- traverse (\ table -> printSolution rel table >> putStrLn ";") tables
                  showTruth (found + length tables) >> run program Idle
    _         -> putStrLn ("Supported commands: terminate (.), find next (;)"
                          ++ ", count (#), find rest ($)")
                  >> run program state

run program Idle = do
  putStr "?- "
  hFlush stdout
  command <- getLine
  if command == "quit"
    then return ()
    else
      case parseRel command of
        Right rel -> run program (Search rel (initTree rel) 0)
        Left err  -> print err >> run program Idle

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile (head args)
      case parseProgram source of
        Right program -> putStrLn "Loaded successfully" >> run program Idle
        Left err -> print err
    _ -> putStrLn "Usage: purelog <filename>"
  return ()

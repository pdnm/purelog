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

printSolution :: Solution -> IO ()
printSolution sol = (putStr . (++ " ") . show $ sol) >> hFlush stdout

findNext :: Machine -> State -> IO ()
findNext machine state@(Search input tree found) =
  case search machine input tree 0 of
    Just (sol, Just tree')  -> printSolution sol >> run machine (Search input tree' (found + 1))
    Just (sol, Nothing)     -> printSolution sol >> putStrLn "." >> showTruth (found+1) >> run machine Idle
    Nothing                   -> putStrLn "." >> showTruth found >> run machine Idle

run :: Machine -> State -> IO ()
run machine state@(Search input tree found) = if found == 0 then findNext machine state else do
  command <- getLine
  case command of
    "."       -> showTruth found >> run machine Idle
    ";"       -> findNext machine state
    "#"       -> putStrLn ("Found " ++ show found ++ " solution(s).") >> run machine state
    "$"       -> do
                  let sols = (searchAll machine input tree)
                  mapM_ (\ sol -> printSolution sol >> putStrLn ";") sols
                  showTruth (found + length sols) >> run machine Idle
    _         -> putStrLn ("Supported commands: terminate (.), find next (;)"
                          ++ ", count (#), find rest ($)")
                  >> run machine state

run machine@(prog, strat) Idle = do
  putStr "?- "
  hFlush stdout
  command <- getLine
  case command of
    "quit"          -> return ()
    "strategy(BFS)." -> run (prog, BFS) Idle
    "strategy(DFS)." -> run (prog, DFS) Idle
    _               ->
      case parseRel command of
        Right input -> run machine (Search input (initTree input) 0)
        Left err  -> print err >> run machine Idle


main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      source <- readFile (head args)
      case parseProgram source of
        Right machine -> putStrLn "Loaded successfully" >> run (machine, DFS) Idle
        Left err -> print err
    _ -> putStrLn "Usage: puinputog <filename>"
  return ()

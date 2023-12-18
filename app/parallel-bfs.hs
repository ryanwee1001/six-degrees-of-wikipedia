module Main (main) where

import Lib (DirectedGraph, Query, addEdge, emptyGraph, runQueries)

import Control.Monad.Par (runPar)
-- import System.Environment (getArgs)
-- import System.Exit (die)

sampleGraph :: DirectedGraph
sampleGraph =
  addEdge 0 1 $
  addEdge 0 2 $
  addEdge 1 2 $
  addEdge 1 3 $
  addEdge 2 4 $
  addEdge 3 4 $
  emptyGraph

sampleQueries :: [Query]
sampleQueries = [(0, 4), (0, 4), (0, 4), (1, 2), (1, 4)]

main :: IO ()
main = do
    putStrLn $ show $ runPar $ runQueries sampleGraph sampleQueries
    -- args <- getArgs
    -- case args of
    --     [filename] -> do
    --         _ <- readFile filename
    --         putStrLn "done"
    --     _ -> do
    --         die "Usage: stack run <filename>"

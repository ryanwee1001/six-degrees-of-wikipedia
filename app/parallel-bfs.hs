module Main (main) where

import Lib (
    runQueries,
    readBinaryFileToGraph,
    readJSONFileToNodes,
    readCSVFileToQueries)

import qualified Data.Map as Map

import Control.Monad.Par (runPar)
import System.CPUTime (getCPUTime)

{- ******** START DATA FILES ******** -}

edgesFile :: FilePath
edgesFile = "data/wikigraph.edges"

nodesFile :: FilePath
nodesFile = "data/wikigraph.nodes"

queriesFile :: FilePath
queriesFile = "data/wikigraph.queries"

{- ******** END DATA FILES ******** -}

forceEvaluationOfMap :: Map.Map k v -> ()
forceEvaluationOfMap m = Map.foldlWithKey (\_ k v -> k `seq` v `seq` ()) () m

forceEvaluationOfList :: [a] -> ()
forceEvaluationOfList l = foldl (\_ x -> x `seq` ()) () l

main :: IO ()
main = do
    edges <- readBinaryFileToGraph edgesFile
    nodes <- readJSONFileToNodes nodesFile
    queries <- readCSVFileToQueries queriesFile nodes

    -- Force the full evaluation of edges / nodes / queries, so that we don't
    -- include I/O in our timing results.
    let resE = forceEvaluationOfMap edges
    let resN = forceEvaluationOfMap nodes
    let resQ = forceEvaluationOfList queries
    putStrLn $ "Edges loaded: " ++ show (resE == ())
    putStrLn $ "Nodes loaded: " ++ show (resN == ())
    putStrLn $ "Queries loaded: " ++ show (resQ == ())

    start <- getCPUTime
    putStrLn $ "Results: " ++ (show $ runPar $ runQueries edges queries)
    end <- getCPUTime
    let runtime = (end - start) `div` 1000000 -- us
    putStrLn $ "Runtime: " ++ show runtime ++ " us"

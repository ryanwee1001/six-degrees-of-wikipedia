module Main (main) where

import Lib (
    runQueries,
    readBinaryFileToGraph,
    readJSONFileToNodes,
    readCSVFileToQueries)

import qualified Data.Map as Map

-- import Control.DeepSeq (deepseq, NFData)
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

-- forceEvaluationOfMap :: (NFData k, NFData v) => Map.Map k v -> ()
-- forceEvaluationOfMap m = Map.foldlWithKey (\_ k v -> k `deepseq` v `deepseq` ()) () m

-- forceEvaluationOfList :: (NFData a) => [a] -> ()
-- forceEvaluationOfList l = foldl (\_ x -> x `deepseq` ()) () l

main :: IO ()
main = do
    graph <- readBinaryFileToGraph edgesFile
    nodes <- readJSONFileToNodes nodesFile
    queries <- readCSVFileToQueries queriesFile nodes

    -- Force the full evaluation of graph / nodes / queries, so that we don't
    -- include I/O in our timing results.
    --
    -- TODO: Don't rely on Map.size / length, which might not fully evaluate the
    --       data structures.
    putStrLn $ "Loaded " ++ (show $ Map.size graph) ++ " Edges"
    putStrLn $ "Loaded " ++ (show $ Map.size nodes) ++ " Nodes"
    putStrLn $ "Loaded " ++ (show $ length queries) ++ " Queries"
    -- let _ = (forceEvaluationOfMap graph) `deepseq` ()
    -- let _ = (forceEvaluationOfMap nodes) `deepseq` ()
    -- let _ = (forceEvaluationOfList queries) `deepseq` ()

    start <- getCPUTime
    putStrLn $ show $ runPar $ runQueries graph queries
    end <- getCPUTime
    let runtime = (end - start) `div` 1000000 -- us
    putStrLn $ "Runtime: " ++ show runtime ++ " us"

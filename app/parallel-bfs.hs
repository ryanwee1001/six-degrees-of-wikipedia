module Main (main) where

import Lib (
    runQueries,
    readBinaryFileToGraph,
    readJSONFileToNodes,
    readCSVFileToQueries)

import Control.Monad.Par (runPar)

{- ******** START DATA FILES ******** -}

edgesFile :: FilePath
edgesFile = "data/wikigraph.edges"

nodesFile :: FilePath
nodesFile = "data/wikigraph.nodes"

queriesFile :: FilePath
queriesFile = "data/wikigraph.queries"

{- ******** END DATA FILES ******** -}

main :: IO ()
main = do
    graph <- readBinaryFileToGraph edgesFile
    nodes <- readJSONFileToNodes nodesFile
    queries <- readCSVFileToQueries queriesFile nodes
    putStrLn $ show $ runPar $ runQueries graph queries

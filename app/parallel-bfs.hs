module Main (main) where

import Lib (
    Query,
    edgesFile,
    -- nodesFile,
    -- queriesFile,
    runQueries,
    readBinaryFiletoIO)

import Control.Monad.Par (runPar)

sampleQueries :: [Query]
sampleQueries = [(3334, 3431), (3431, 730813), (3334, 730813), (3334, 0)]
-- Should give: 1, 1, 2, -1

main :: IO ()
main = do
    graph <- readBinaryFiletoIO edgesFile
    putStrLn "Finished reading graph into memory!"
    putStrLn $ show $ runPar $ runQueries graph sampleQueries

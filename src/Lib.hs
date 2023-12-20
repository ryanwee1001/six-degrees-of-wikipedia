module Lib
    ( runQueries,
      readBinaryFileToGraph,
      readJSONFileToNodes,
      readCSVFileToQueries
    ) where

import Control.Monad (replicateM)
import Control.Monad.Par (Par, parMap, parMapM)
import Data.Binary (Word32)
import Data.Binary.Get (getWord32le, runGet)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified NodeParser

import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified System.IO as IO

type Node = Int
type DirectedGraph = Map.Map Node (Set.Set Node)    -- adjacency list
type Query = (Int, Int)                             -- from, to

{- ******** FOR PARALLELIZING BFS ******** -}

getNeighbors :: DirectedGraph -> Node -> Set.Set Node
getNeighbors graph node = fromMaybe Set.empty $ Map.lookup node graph

-- Does union on a list of sets in parallel
parallelUnion :: Ord a => [Set.Set a] -> Par (Set.Set a)
parallelUnion [] = do
    return Set.empty
parallelUnion [s] = do
    return s
parallelUnion sets = do
    let (half1, half2) = splitAt (length sets `div` 2) sets
    mergedHalf1 <- parallelUnion half1
    mergedHalf2 <- parallelUnion half2
    return $ Set.union mergedHalf1 mergedHalf2

{-
    Recursive function for BFS that is executed in parallel.

    @param graph    graph represented as an adjacency matrix
    @param frontier nodes in current frontier
    @param visited  nodes that have been visited (including those in frontier)
    @param dist     distance to nodes in the current frontier
    @param target   target node

    Note that we limit our BFS depth to 6, so that we don't go on forever.
-}
parallelBFS :: DirectedGraph -> [Node] -> Set.Set Node -> Int -> Node -> Par Int
parallelBFS _ _ _ 6 _ = do
    return (-1)
parallelBFS graph frontier visited dist target = do
    neighbors <- parMap (getNeighbors graph) frontier
    tmpFrontier <- parallelUnion neighbors
    let nextFrontierSet = Set.difference tmpFrontier visited 
    if null nextFrontierSet then
        return (-1)
    else do
        if Set.member target nextFrontierSet then
            return (dist + 1)
        else do
            let newVisited = Set.union visited nextFrontierSet
            let newFrontier = Set.toList nextFrontierSet
            parallelBFS graph newFrontier newVisited (dist + 1) target

{-
    Does BFS to find single-source shortest path.
-}
parallelBFSDriver :: DirectedGraph -> Query -> Par Int
parallelBFSDriver graph query = do
    parallelBFS graph [fst query] Set.empty 0 (snd query)

{- ******** FOR PARALLELIZING QUERIES ******** -}

runQueries :: DirectedGraph -> [Query] -> Par [Int]
runQueries graph queries = do
    parMapM (parallelBFSDriver graph) queries

{- ******** FOR PARSING EDGES ******** -}

-- TODO: Optimize these!

intListToTupleList :: [Int] -> Int -> Bool -> [(Int, Set.Set Int)]
intListToTupleList [] _ _ =[]
intListToTupleList (x:ns) cur_idx del | del = intListToTupleList ns (cur_idx+1) False
                                      | otherwise = (cur_idx, Set.fromList neighbors) :
                                                intListToTupleList xs cur_idx True
                                            where (neighbors,xs) = span (/=0) (x:ns)

parseListToMap :: [Word32] -> Map.Map Int (Set.Set Int)
parseListToMap x = Map.fromList
                    (intListToTupleList (Prelude.map fromIntegral x) 1 False)

readBinaryFileToGraph :: FilePath -> IO (Map.Map Int (Set.Set Int))
readBinaryFileToGraph filePath = do
    -- Read the binary file into a lazy ByteString
    fileContents <- BL.readFile filePath
    return $ parseListToMap $
        runGet (replicateM (fromIntegral $ BL.length fileContents `div` 4)
        getWord32le) fileContents

{- ******** FOR PARSING NODES ******** -}

-- TODO: Optimize these!

readJSONFileToNodes :: FilePath -> IO (Map.Map String Int)
readJSONFileToNodes f = do
  n <- NodeParser.parseFile f
  return $ case n of
    Left err -> error err
    Right n2 -> NodeParser.nodes n2

{- ******** FOR PARSING QUERIES ******** -}

-- TODO: Optimize these!

{-
    Queries should be represented as (<from-page>,<to-page>)

    Outputs Nothing if:
        (a) Query is malformed, e.g. (node1,node2,node3)
        (b) "From" page or "To" page doesn't exist

    TODO: Match page names in a case-insensitive manner?
-}
convertStringsToQuery :: Map.Map String Int -> [String] -> Maybe Query
convertStringsToQuery m [from, to]
    | Map.member from m && Map.member to m = 
            Just (fromMaybe 0 $ Map.lookup from m, fromMaybe 0 $ Map.lookup to m)
    | otherwise = Nothing
convertStringsToQuery _ _ = Nothing

{-
    Populates a list of queries using the corresponding string representations.
-}
readCSVFileToQueriesHelper :: Map.Map String Int -> [Query] -> [String] -> [Query]
readCSVFileToQueriesHelper m acc listElem = 
    fromMaybe (0, 0) (convertStringsToQuery m listElem) : acc

readCSVFileToQueries :: FilePath -> Map.Map String Int -> IO [Query]
readCSVFileToQueries filePath m = do
    fileContents <- IO.readFile filePath
    let queryLines = lines fileContents
    let queryStrings = map (splitOn ",") queryLines -- [[String]]
    return $ foldl (readCSVFileToQueriesHelper m) [] queryStrings

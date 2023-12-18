module Lib
    ( Node,
      DirectedGraph,
      Query,
      addEdge,
      emptyGraph,
      runQueries,
    ) where

import Control.Monad.Par (Par, parMap, parMapM)
import Data.Maybe (fromMaybe)

import qualified Data.Set as Set
import qualified Data.Map as Map

type Node = Int
type DirectedGraph = Map.Map Node (Set.Set Node)    -- adjacency list
type Query = (Int, Int)                             -- from, to

{- ******** FOR PARALLELIZING BFS ******** -}

addEdge :: Node -> Node -> DirectedGraph -> DirectedGraph
addEdge from to = Map.insertWith Set.union from (Set.singleton to)

emptyGraph :: DirectedGraph
emptyGraph = Map.empty

getNeighbors :: DirectedGraph -> Node -> Set.Set Node
getNeighbors graph node = fromMaybe Set.empty $ Map.lookup node graph

{-
    Recursive function for BFS that is executed in parallel.

    @param graph    graph represented as an adjacency matrix
    @param frontier nodes in current frontier
    @param visited  nodes that have been visited (including those in frontier)
    @param dist     distance to nodes in the current frontier
    @param target   target node
-}
parallelBFS :: DirectedGraph -> [Node] -> Set.Set Node -> Int -> Node -> Par Int
parallelBFS graph frontier visited dist target = do
    neighbors <- parMap (getNeighbors graph) frontier
    let tmpFrontier = Set.unions neighbors
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

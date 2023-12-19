import Lib (DirectedGraph, Query, addEdge, emptyGraph)

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
main = putStrLn "Test suite not yet implemented"

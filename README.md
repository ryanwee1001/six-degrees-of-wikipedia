# Six Degrees of Wikipedia

## Introduction

See the [proposal](./proposal.pdf) for more details.

## Dataset

The program requires three data files:
- \<edgesFile\>: The edges that form the graph
- \<nodesFile\>: The mapping from node names to node integer IDs
- \<queriesFile\>: The queries to execute

The filepaths for these three files are hardcoded in [Lib.hs](./src/Lib.hs).

> Note: The current implementation doesn't use \<nodesFile\>, because it operates on \<int, int\> queries.

> Note: The current implementation doesn't use \<queriesFile\>, because it operates on a set of hardcoded queries in [parallel-bfs.hs](./app/parallel-bfs.hs).

## Usage

To run the parallelized BFS, use:

```bash
stack build
stack run +RTS -N<num-cores>
```

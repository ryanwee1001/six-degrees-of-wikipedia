# Six Degrees of Wikipedia

## Introduction

See the [proposal](./proposal.pdf) for more details.

## Dataset

The program uses three data files:
- \<edgesFile\>: The edges that form the graph
- \<nodesFile\>: The mapping from node names to node integer IDs
- \<queriesFile\>: The queries to execute

The filepaths for these three files are hardcoded in [Lib.hs](./src/Lib.hs).

## Usage

To run the parallelized BFS, use:

```bash
stack build
stack run +RTS -N<num-cores>
```

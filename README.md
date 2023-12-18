# Six Degrees of Wikipedia

## Introduction

See the [proposal](./proposal.pdf) for more details.

## Dataset

Currently, the BFS is done on a sample graph hardcoded in [parallel-bfs.hs](./app/parallel-bfs.hs).

## Usage

To run the parallelized BFS, use:

```bash
stack build
stack run +RTS -N<num-cores>
```

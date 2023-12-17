# Six Degrees of Wikipedia

## Introduction

See the [proposal](./proposal/proposal.pdf) for more details.

## Dataset

The graph should be represented as an adjacency matrix, where nodes are 0-indexed and each line contains the adjacencies as a comma-separated string.

Refer to the [sample data](./sample-data.txt) for more details.

## Usage

To run the non-parallel BFS, use:

```bash
stack --resolver lts-21.9 ghc -- --make -Wall -O naive-bfs
./naive-bfs <filename>
```
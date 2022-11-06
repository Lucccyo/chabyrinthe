# chabyrinthe
Generating maze with different kind of algorithms

## Binary tree maze generation

A binary tree maze is a standard orthogonal maze where each cell always 
To create a binary tree maze, for each cell flip a coin to decide 
has a passage leading up or leading left, but never both. 
whether to add a passage leading up or left. 

> ocamlopt binary_tree/maze.ml -o maze && ./maze 14 14

```
|___  |  |  |___  |  |  |  |  |  |___  |  |
  |  |___  |  |  |___  |___  |  |___  |  |___
  |___  |______  |  |  |___  |  |  |  |______
______  |______  |___  |___  |  |___  |  |___
  |___  |___  |  |  |___  |  |______  |  |___
___  |  |___  |  |  |___  |______  |  |___  |
___  |  |___  |___  |  |  |  |______  |___  |
  |  |___  |______  |  |_________  |  |___  |
  |______  |___  |______  |___  |______  |___
  |  |___  |___  |  |  |___  |  |___  |______
  |  |___  |______  |  |___  |___  |  |______
  |  |  |  |______  |  |  |_________  |___  |
  |  |  |______  |  |_____________________  |
___  |  |___  |  |___  |______  |  |___  |  |
  |___  |___  |  |  |  |  |  |___  |___  |  |
```

## Randomized Prim

Start with a grid full of walls.
  - Pick a cell, mark it as part of the maze. Add the walls of the cell to the wall list.
  - While there are walls in the list:
  - Pick a random wall from the list. If only one of the cells that the wall divides is visited, then:
  - Make the wall a passage and mark the unvisited cell as part of the maze.
  - Add the neighboring walls of the cell to the wall list.
  - Remove the wall from the list.



  ___
 /   \
 \___/
     \___

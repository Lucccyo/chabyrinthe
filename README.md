# chabyrinthe
Generating maze with different kind of algorithms

## Binary tree maze generation

A binary tree maze is a standard orthogonal maze where each cell always 
To create a binary tree maze, for each cell flip a coin to decide 
has a passage leading up or leading left, but never both. 
whether to add a passage leading up or left. 

> ocamlopt binary_tree/tree.ml -o tree && ./tree 15 15 9

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

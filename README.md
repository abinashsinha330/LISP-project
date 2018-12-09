# LISP-project
1. Implemented A* search and used it to solve 8-puzzle using the same:

Execute the command:
clisp eightpuzzle.lisp 0 1 3 4 2 5 7 8 6

where the arguments in the order given indicate the starting state of the puzzle in row-major format
So it basically looks like
0 1 3
4 2 5
7 8 6


2. Implemented hill-climbing search and used it to solve N-queens problem:

Execute the command:
clisp eightqueens.lisp N

where N is the board size for which we want to solve N queens placement problem. The final goal state should be such that no queen is able to attack each other

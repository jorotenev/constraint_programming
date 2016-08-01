The sudoku puzzles are in the `sudoku_problems.pl` file.  
Two different viewpoints of solving the puzzles are here - `chrVP1_sudoku.pl` and `chrVP2_sudoku.pl`. 
VP1 (viewpoint #1) is the classical viewpoint and VP2 uses sets (http://gecoder.org/examples/sudoku-set.html).

Use the `./run_script.sh` script to solve all puzzles.  
`./run_script.sh <iteration:any> <timeout:int> <viewpoint:[VP1|VP2]>`  
`<iteration>` is used to mark different versions of the program being ran (handy to compare the efficiency at different stages of the implementation). It will be used to name the output directory for the log.
`<timeout>` is the runtime limit in seconds, which each puzzle is allowed to run for, before moving on to the next puzzle.
`<viewpoint>` is the viewpoint to use (VP1 and VP2 are the current valid options).  
Example usage  
`./run_script.sh boo 60 VP2` which will run the `chrVP2_sudoku.pl` file with a 60 second timeout limit for each puzzle, and the resutls will be stored under VP2/log_boo


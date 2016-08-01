# Solver for the Shikaku puzzles.  
Example puzzles are in the shikakuproblems.pl file.  
To run a single puzzle (assuming Swi Prolog is used):  
`$ swipl -s chr_shikaku.pl -t 'solve(helmut)'`  
`helmut` can be replaced with any puzzle from the shikakuproblems.pl file.  

Use the `run_script.sh` to run all puzzles  
`$ ./run_script.sh <iteration:any> <timeout:int>`  
iteration - a symbol(s) to mark the version of the solver. handy to compare the performance at the different stages when implementing
timeout - in seconds, the allowed period of time to solve a puzzle, before moving on to the next one.  
Example usage:  
`$ ./run_script.py 1 60` - the output will be in `log/log_1`, and the timeout limit is 60 seconds.


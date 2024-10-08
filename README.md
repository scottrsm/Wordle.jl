# Wordle.jl
Solves the NYT Wordle puzzle.
To be successful, one has to find the hidden word by no more than six guesses.
The solver has been designed to work well on words that are
used more frequently without sacrificing overall performance.

Below we examine 
- Overall Performance -- We consider all trials, even if a "solve" took more than six guesses.
- When Successful -- We only consider the "solves" where the number of 
guesses were less than or equal to six.

The stats for the solver are (based on 3585 five letter words):
- Overall          : The mean number of guesses to solve: 4.36.
- Overall          : The mean number of guesses (weighted by word usage frequency) to solve: 2.79.
- When Successful  : The mean number of guesses to solve: 4.31.
- When Successful  : The mean number of guesses (weighted by word usage frequency) to solve: 2.79.
- When Unsuccessful: The mean number of guesses to complete: 7.51.
- When Unsuccessful: The mean number of guesses to complete (weighted by word usage frequency): 7.04.
- Percent unsuccessful: 1.53%.
- Percent unsuccessful (weighted by word usage frequency): 0.089%.


# Wordle.jl

[![Docs](https://img.shields.io/badge/docs-dev-blue.svg)](https://scottrsm.github.io/Wordle.jl/dev/)

Solves the NYT Wordle puzzle.
To be successful, one has to find the hidden word by no more than six guesses.
The solver has been designed to work well on words that are
used more frequently without sacrificing overall performance.

Below we examine 
- Overall Performance -- We consider all trials, even if a "solve" took more than six guesses.
- When Successful -- We only consider the "solves" where the number of 
guesses were less than or equal to six.

The stats for the solver are (based on the 3591 five letter words in the database):
- Overall          : The mean number of guesses to solve: 4.14.
- Overall          : The mean number of guesses (weighted by word usage frequency) to solve: 2.69.
- When Successful  : The mean number of guesses to solve: 4.01.
- When Successful  : The mean number of guesses (weighted by word usage frequency) to solve: 2.69.
- When Unsuccessful: The mean number of guesses to complete: 7.56.
- When Unsuccessful: The mean number of guesses to complete (weighted by word usage frequency): 7.36.
- Percent unsuccessful: 3.62%.
- Percent unsuccessful (weighted by word usage frequency): 0.064%.

## Documentation
- HTML (latest, built from `main`): https://scottrsm.github.io/Wordle.jl/dev/
- Markdown source: [docs/src/index.md](docs/src/index.md)

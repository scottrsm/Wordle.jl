# Wordle.jl Documentation

```@meta
CurrentModule = Wordle
```

# Overview
This module contains a New York Times `Wordle` solver.
It is optimized to solve `Wordle` puzzles based on most
frequently used 5 letter words.
It provides the number of steps to solve along with the
intermediate guesses.

There is an associated Wordle Jupyter notebook at 
julia/src/WordleTest.ipynb.


## Primary Function

```@docs
solve_wordle
```

## Supporting Functions

```@docs
create_wordle_info
```

```@docs
filter_universe
```

```@docs
freq_letter_strat
```


## Index

```@index
```


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
src/WordleTest.ipynb.


# Solver Strategy
- Initial Conditions:
  - Set puzzle\_word
  - X = Start with universe of 5 letter words along with freqency of usage.
  - Set current\_universe = X
 - Start
  - Pick guess (by default use the function `get_next_word`).
  - If guess == puzzle\_word
    - Goto End
  - Get wordle match info about how close guess is to the correct word:
    - wordle\_info = create\_wordle\_info(<guess>, <puzzle\_word>)
      - **Example:** wordle\_info, create\_wordle\_info("exact", "crane") =
      ([('a', 3), ('e', -1), ('c', -4)], Dict('x' => (0, 0), 'c' => (1, 1), 'e' => (1, 1), 't' => (0, 0)))
  - Use this match info to filter existing universe of words.
    - current\_universe = filter\_universe(wordle\_info, current\_universe)
  - Goto Start
- End
- Return guess


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

```@docs
get_next_word
```

## Index

```@index
```


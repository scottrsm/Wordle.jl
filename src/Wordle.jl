module Wordle

export create_wordle_info, filter_universe, freq_letter_strat, get_next_word, solve_wordle

import CSV
import StatsBase
using DataFrames
using InlineStrings

struct NotSorted <: Exception
    var::String
end

Base.show(io::IO, e::NotSorted) = print(io, "Words are NOT sorted by $(e.var)")

# LFA is an ordering of the alphabet based on letter frequency
# from some corpus of text.
const LFA = collect("etaoinshrdlcumwfgypbvkjxqz")

# Load Wordle database -- stored as a CSV file.
const WORDLE_DF = DataFrame(CSV.File(joinpath(@__DIR__, "../data", "wordle_db.csv");
    header=6,
    comment="#"))


"""
    create_wordle_info(guess, pword)

Create an information structure of the form:
    `([LETTER, EXACT_MATCH_POSITION)], Dict(LETTER => (NUMBER_OF_MATCHES, MATCH_FLAG))`

Here, the dictionary has the inexact match information: of words.
- LETTER : A matching letter
- EXACT\\_MATCH\\_POSITION: The position (1-based index) if an exact match; OR
        minus the position if the letter is used, but not at this position.
- NUMBER\\_OF\\_MATCHES : The number of matches.

The latter is interpreted thusly:
- If MATCH\\_FLAG is 0: There are *exactly* NUMBER\\_OF\\_MATCHES with this letter that should occur in	the puzzle word in addition to any existing exact matches of this letter.
- Else (flag is 1)    : There are *at least* NUMBER\\_OF\\_MATCHES with this letter that should occur in the puzzle word in addition to any existing exact matches of this letter.

# Type Constraints
- T <: AbstractString

# Arguments
- `guess::T`: The guess for the puzzle.
- `pword::T`: The puzzle word.


# Returns
    A tuple of a vector of tuples of exact matches and a dictionary of
    inexact match info.

# Examples
```jdoctest
julia> create_wordle_info("which", "where")
([('w', 1), ('h', 2)], Dict('h' => (0, 0), 'c' => (0, 0), 'i' => (0, 0)))
```

```jdoctest
julia> create_wordle_info("teens", "where")
([('e', 3), ('e', -2)], Dict('n' => (0, 0), 's' => (0, 0), 't' => (0, 0), 'e' => (1, 1)))
```
"""
function create_wordle_info(guess::T, # Guess
                            pword::T, # Puzzle word
                                     )::Tuple{Vector{Tuple{Char,Int}},Dict{Char,Tuple{Int,Int}}} where {T<:AbstractString}
    n = length(pword)
    e_idx = Int[]
    f_idx = collect(1:n)
    c_idx = Int[]

    ary::Vector{Tuple{Char,Int}} = []

    # Push exact info onto `ary`.
    for i in 1:n
        if guess[i] == pword[i]
            push!(ary, (guess[i], i))
            push!(e_idx, i)
        end
    end

    c_idx = setdiff(f_idx, e_idx)

    dp = Dict{Char,Int}()
    dg = Dict{Char,Int}()
    for i in c_idx
        dp[pword[i]] = 1 + get(dp, pword[i], 0)
        dg[guess[i]] = 1 + get(dg, guess[i], 0)
    end

    # Dictionary
    d = Dict{Char,Tuple{Int,Int}}()
    for i in c_idx
        guess_letter_count = dg[guess[i]]
        guess_letter_count_in_puzzle = get(dp, guess[i], 0)


        #= If number of times the letter guess[i] is seen in the puzzle is greater than or equal
           to the number of times it occurs in the guess word, then every such letter
           in the guess word will be recognized as an inexact match.
        =#
        if dg[guess[i]] <= guess_letter_count_in_puzzle
            d[guess[i]] = (guess_letter_count, 1)
        else # Otherwise, only `guess_letter_count_in_puzzle` number of this letter will "light up" as an inexact match.
            d[guess[i]] = (guess_letter_count_in_puzzle, 0)
        end
        # Mark this position as an inexact match.
        if guess_letter_count_in_puzzle > 0
            push!(ary, (guess[i], -i))
        end
    end

    return ((ary, d))
end

"""
    filter_universe(wordle_info, words)

Filter an existing universe of words based on match info.

# Type Constraints
- T <: AbtractString

# Arguments
- `wordle_info` : Wordle info of the form:
                    `([(LETTER, EXACT_POSITION)], Dict( LETTER => (k, n)))`
                  The Wordle info -- the same type as the return value from
                  `create_wordle_info`.
- `words`       : A Vector of words of type `T`.

# Return
    A subset of the `words` vector based on the filter information
    from `wordle_info`.

# Examples
```jdoctest
julia> (winfo, d) = create_wordle_info("which", "where")

([('w', 1), ('h', 2)], Dict('h' => (0, 0), 'c' => (0, 0), 'i' => (0, 0)))

julia> words = ["state", "which", "where", "child", "there", "taste"]

julia> filter_universe((winfo, d), words)

1-element Vector{T}:
 "where"
```
"""
function filter_universe(wordle_info::Tuple{Vector{Tuple{Char,Int}},Dict{Char,Tuple{Int,Int}}},
    words::Vector{T},
)::Vector{T} where {T<:AbstractString}

    # Nothing left to filter.
    if length(words) == 0
        return (words)
    end

    # Destructure the `worlde_info`, get the length of the words
    # used in word lists.
    (winfo, d) = wordle_info
    word_len = length(words[1])

    # This is the list of all the indices in any given puzzle word.
    f_idxs = collect(1:word_len)

    # Filter words on exact matches...
    ems = map(x -> x[2], winfo)
    e_idxs = [i for i in ems if i > 0] # Exact match
    ie_idxs = [-i for i in ems if i < 0] # In-exact match
    c_idxs = setdiff(f_idxs, union(e_idxs, ie_idxs))

    if length(e_idxs) > 0
        cstr = T(String([ci[1] for ci in winfo if ci[2] > 0]))
        words = filter(word -> cstr == word[e_idxs], words)
    end

    if length(ie_idxs) > 0
        cstr = T(String([ci[1] for ci in winfo if ci[2] < 0]))
        words = filter(word -> cstr != word[ie_idxs], words)
    end

    # These are the indices of potential inexact matches.
    c_idx = setdiff(f_idxs, e_idxs)
    m = length(c_idx)

    # Adjust filtering based on match flag `(d[k][2])`.
    if m > 0
        for k in keys(d)
            fil = fill(k, m)
            if d[k][2] == 0
                words = filter(word -> sum(collect(word[c_idx]) .== fil) == d[k][1], words)
            else
                words = filter(word -> sum(collect(word[c_idx]) .== fil) >= d[k][1], words)
            end
        end
    end

    # Return the filtered words.
    return (words)
end


"""
    freq_letter_strat(swords, lfa, c_idx)

Strategy to pick a guess for Wordle.

The strategy consists of the following:
- Take the words in the current universe.
- Take the complement of the indices where we have exact information.
      For each of these indices create a dictionary with letter => count.
- Pick the index where the corresponding dictionary has the largest count
        value for some letter.
- If for a given dictionary, there are several letters with the same
        count, pick the letter from the letter freq string below.
- Do the same now across dictionaries, find the letter that is the most
        frequent and its dictionary index.
- For this index, find all words with this letter in this slot.
        Pick the one that is most frequent.
        This will be our guess.

# Type Constraints
- T <: AbtractString

# Arguments
- `swords::AbstractVector`    : A Vector of sorted strings (sorted by frequency of occurrence).
- `lfa::Vector{Char}`         : This is the alphabet in lower case as a character vector from
                                most to least used.
- `c_idx::Vector{Int}`      : This is the index values of words to analyze.
                This list is usually the complement
                of exact match indices from a previous guess.

# Return

    A guess word.

# Assumes

    The characters in swords are lowercase letters: [a-z].

"""
function freq_letter_strat(swords::AbstractVector{T}, # The sorted list of words to choose from.
    lfa::Vector{Char}, # The letter frequency order of the alphabet.
    c_idx::Vector{Int}, # The complement of the indices that are exact.
)::T where {T<:AbstractString}

    # Create corresponding dictionaries for each index.
    ds = [Dict{Char,Int}() for _ in c_idx]
    ary = []

    # Fill each of the dicts: at index
    # `i`, `ds[i]`: char => count (using swords)
    for i in 1:length(c_idx)
        for word in swords
            ds[i][word[c_idx[i]]] = 1 + get(ds[i], word[c_idx[i]], 0)
        end
    end

    # Fill the array `ary` with tuples of the form:
    # `(idx, char, num_of_occurrences, lfa_order)`
    for i in 1:length(c_idx)
        mx = maximum(values(ds[i]))
        for (k, v) in ds[i]
            if v == mx
                push!(ary, (c_idx[i], k, v, (findall(x -> x == k, lfa))[1]))
            end
        end
    end

    # Sort `ary` by occurrence followed by `lfa` order.
    sary = sort(ary, lt=((x, y) -> (x[3] < y[3]) | (x[3] == y[3] & (x[4] > y[4]))), rev=true)

    # Get the index and character of the most frequent/most-used character.
    idx = sary[1][1]
    c = sary[1][2]

    # Return the first word(which is sorted by frequency of occurrence)
    #  which has character `c` at index, `idx`.
    return ((filter(x -> x[idx] == c, swords))[1])
end



"""
	get_next_word(words, wts) 

This function tries to find the "best" guess from the vector of words, `words`.
It does this by taking each word and rearranging the letters in alphabetical order.
It then sorts this list of new "words" lexically, while keeping the associated weights.
This list of words is looped over, forming grouping "words" with corresponding associated 
weight equal to the sum of all proper words that it "contains".

For instance, the words: "ether", "there", and "three" each get translated to the grouping string: "eehrt".
Next, the grouping strings like "eehrt" are sorted. In the case of the string "eehrt", it occurs
three times for the three associated words. The weights of the three occurrences is also kept.
We loop over these grouping strings and add up the associated weights.
Finally, we find the grouping string with the most weight and look for the first match
in the vector, `words`, that belongs to this grouping string. Since `words` is ordered by weight,
we get the word that has the highest weight of the words in its associated grouping string.

# Type Constraints
- T <: AbtractString

# Arguments

- `words::AbstractVector{T}`       : The remaining pool of words to guess from.
- `wts  ::AbstractVector{Float64}` : The usage frequency of the associated words in the vector, `words`. 

#  Input Contract
-  
- `words == words[sortperm(wts, rev=true)]`       ``\\quad`` (Words are sorted from highest to lowest by word *usage*.)

# Return
::Int -- The index of a "best" guess in the user supplied vector  of words.
"""
function get_next_word(words::AbstractVector{T}, wts::AbstractVector{Float64}) :: Int  where {T <: AbstractString}

    # Create a new vector of "words" that rearranges the characters of each word in sorted (lexical) order.
    swrds = map(x -> join(sort(split(x, ""))), words)

    # Now sort this new "word" list (lexically)
    # Use the sort order to order the associated weights.
    idxs  = sortperm(swrds)
    swrds = swrds[idxs]
    swts  = wts[idxs]

    # Create two new vectors: one of "words", `nwrds`, and the other associated weights, `nwts`.
    # These new words are actually "word groupings": For instance, the word, "eehrt", represents the three 5 letter words: "ether, there", and "three".
    # The associated weight of "eehrt" is the sum of the weights associated with "ether", "there", and "three" from the word list weights, `swts`.
    N     = length(swrds)
    nwrds = Vector{T}(undef, N)
    nwts  = Vector{Float64}(undef, N)

	N != 1 || return(1)

    last_str = swrds[1]
    wt = swts[1]
    j = 1
    last_i = 0
    for i in 2:N
        if last_str == swrds[i]
             wt += swts[i]
        else
            nwts[j]  = wt
            nwrds[j] = last_str
            last_str = swrds[i]
            wt = swts[i]
            j += 1
            last_i = i
        end
    end
    nwts[j] = wt
    nwrds[j] = last_str

    # Now find the "word grouping" with the most weight.
    idx = partialsortperm(nwts[1:j], 1; rev=true)
    best_group = nwrds[idx]

    # Find the first word in the original word list that matches the letters in `base_group`.
    # Of all words that match, since the word list is ordered by frequency weight, the first
    # word will be the most likely one to choose.
    # Example: If `best_group` == "eehrt", then words[idx] == "there"
    idx = findfirst(x -> best_group == join(sort(split(x, ""))), words)

    return idx
end


"""
    solve_wordle(puzzle_word, universe_df, rec_count, sol_path, last_guess,
                    lfa; <keyword arguments>)

Solves a NYT Wordle puzzle.

By default, makes guesses based on function, `get_next_word`.
However, there is an option to pass in a guessing strategy function.

# ASSUMES: The universe DataFrame is sorted from highest frequency to lowest.

# Arguments

- `puzzle_word::String`    : The puzzle word.
- `universe_df::DataFrame` : A DataFrame with schema: word(words of the same length),
                             freq(freq fraction by use)

     **NOTE:** The universe is assumed to be sorted in
               reverse order by the :freq column.
- `rec_count::Int`   : The number of calls to this function.
- `sol_path::Vector{Any}`    : Any containing the current list of guesses:
                    `[ (guess, exact_info, universe_size) ...]`
- `last_guess::String`  : The previous guess.
- `lfa::Vector{Char}`         : The lowercase alphabet listed in frequency-of-use order.

# Keyword Arguments

- `chk_inputs::Bool`     : If `true`, check the input contract.
- `guess_strategy::Union{Function,Nothng}` : If not `nothing`, apply this function to pick the next guess.
                     If `nothing`, pick based on the function `get_next_word`.
- `ul::Int`             : The lower threshold size of the filtered Wordle universe.
- `uu::Int`             : The upper threshold size of the filtered Wordle universe.
- `init_guess::String`     : The starting guess to use.

Here,
- `exact_info` has the form: `[(LETTER, POSITION) ...]`
- `universe_size` is the size the word list when the `guess` was made.
- The `guess_strategy` is only turned on when the filtered Wordle universe
  is between the thresholds: `ul` and `uu`; otherwise, the default strategy
  is used -- the most frequently used word in the existing filtered Wordle
  universe is chosen.

#  Input Contract
- `universe_df` schema is (:word, :freq). Define `words`, `freq`, and `N` by:
    - `words = universe_df[:words]`;
    - `freq  = universe[:freq]`;
    - `N     = |universe|`
- `∃ m > 0, ∀ i∈[1,N], |words[i]| == m` ``\\quad`` (All the words in `universe_df` have the same length. )
- `words == words[sortperm(wts, rev=true)]`  ``\\quad`` (Words are sorted from highest to lowest by word *usage*.)

# Return
    (sol_path, number-of-guesses, :SUCCESS/:FAILURE)

   **NOTE:** A sol_path that does not include the puzzle word, means
              that at some point after a guess was made -- along with
              the corresponding filtering of the universe -- there was
              only one word left. In this case the guess count was
              increased by 1, but the function did not recurse.

# Examples
```jdoctest
julia> solve_wordle("taste"; init_guess="their")

(Any[(String7("their"), [('t', 1), ('e', -3)], 3585), 
     (String7("taken"), [('t', 1), ('a', 2), ('e', -4)], 34), 
	 (String7("table"), [('t', 1), ('a', 2), ('e', 5)], 3), 
	 (String7("taste"), [('t', 1), ('a', 2), ('s', 3), ('t', 4), ('e', 5)], 2)], 4, :SUCCESS)
```
"""
function solve_wordle(puzzle_word::String, # Puzzle word.
    universe_df::DataFrame=WORDLE_DF, # Wordle database as DataFrame.
    rec_count::Int=1, # Number of calls (including this one) to this function.
    sol_path::Vector{Any}=[], # The solution path of guessed words, so far.
    last_guess="", # The last guess.
    lfa::Vector{Char}=LFA; # The frequency of use of the alphabet.
    chk_inputs::Bool=true, # Do we check the input contract?
    guess_strategy::Union{Function,Nothing}=nothing, # Function to pick the next guess.
    ul::Int=20, # Used if function guess_strategy given.
    uu::Int=50, # Used if function guess_strategy given.
    init_guess::String="trace", # Starting guess to use.
)::Tuple{Any,Int,Symbol} 

    # Check input contract?
    if chk_inputs && rec_count == 1
        # 0. Get the words from the universe and ensure that we have more than 1.
        words = universe_df[!, :word]
        length(words) <= 1 && throw(DomainError(0, "Their is at most one word in the `universe_df`."))

        # 1. Does `universe_df` have the correct schema?
        Set(names(universe_df)) != Set(["word", "freq"]) && throw(DomainError(0, "The column names of `universe_df` are not correct."))

        # 2. Do :words from `universe_df` have the same length?
        if sum(diff(map(word -> length(word), words))) != 0
            throw(DomainError(0, "Some words in `universe_df` have differing lengths."))
        end

        # 3. Check that the words are already sorted from highest to lowest.
        sidx = sortperm(universe_df[!, :freq], rev=true)
        words[sidx] != words && throw(NotSorted("`words`: Not sorted from hightest to lowest by usage frequency"))
    end

    puzzle_word = String7(puzzle_word)
	last_guess = String7(last_guess)
	init_guess = String7(init_guess)

    # Get a reference to the Wordle universe.
    univs = Array(universe_df[!, :word])
    uwts = Array(universe_df[!, :freq])

    # Current guessing strategy is to take the most frequently used word
    #  in the current universe -- except for the very first guess.
	guess = String7(univs[1])
    if last_guess == ""
		guess = String7(init_guess)
    else
        univs = filter(x -> x != last_guess, univs)
        if length(univs) == 0
            return ((sol_path, rec_count, :FAILURE))
        end
        # Get the most frequent word from the new filtered list of words.
        idx = get_next_word(univs, uwts)
        guess = String7(univs[idx])
    end
    word_len = length(guess)

    #= If we specified a picking strategy, modify the guess.
       -- Only used after first guess (last_guess != "").
       The strategy is based on:
       1. The existing universe
       2. The letter frequency order.
       3. The indices to focus on.
    =#
    if (guess_strategy !== nothing) && (last_guess != "")
        if length(sol_path) != 0
            exact_info = sol_path[end][2]
            ulen = length(univs)
            if length(exact_info) != 0 && ul < ulen && ulen < uu
                f_idx = collect(1:word_len)
                e_idx = map(x -> x[2], exact_info)
                c_idx = setdiff(f_idx, e_idx)
                guess = guess_strategy(univs, lfa, c_idx)
            end
        end
    end

	# See the documentation for create_wordle_info.
    (exact_info, ino_dct) = create_wordle_info(guess, puzzle_word)

    # Get the size of the current search universe.
    # Push the guess; the "exact match info"; and the size of the universe
    # onto the `sol_path`.
    n = length(univs)
    push!(sol_path, (guess, exact_info, n))

    # if we guessed the puzzle word, return success.
	if guess == puzzle_word && rec_count <= 6
        return ((sol_path, rec_count, :SUCCESS))
	elseif guess == puzzle_word
        return ((sol_path, rec_count, :FAILURE))
    end

    # Filter the current universe based on the match info to get the new universe.
    new_universe = filter_universe((exact_info, ino_dct), univs)

    # Look at the size of the new universe -- we can make conclusions in some instances.
    n = length(new_universe)
    if n == 0 # The information does not lead to a solution -- the puzzle word is not in our initial universe.
        return ((sol_path, rec_count, :FAILURE))
    elseif n == 1 && puzzle_word == new_universe[1] # We know the solution without having to recurse again.
        return ((sol_path, rec_count + 1, :SUCCESS))
    elseif n == 1 # The puzzle word is not in our initial universe.
        return ((sol_path, rec_count + 1, :FAILURE))
    end

    # If we recursed too much, there must be an error.
    if rec_count > (10 * word_len)
        return ((sol_path, rec_count, :FAILURE))
    end

    # Get the new universe as a dataframe and sort it based on frequency
    # of occurrence from highest to lowest.
    nuniv_df = filter(:word => x -> x in new_universe, universe_df)
    sort!(nuniv_df, order(:freq, rev=true))

    # Recurse...
	solve_wordle(String(puzzle_word), nuniv_df, rec_count + 1, sol_path, String(guess),
        lfa; chk_inputs=false, guess_strategy=guess_strategy)
end

end # module Wordle

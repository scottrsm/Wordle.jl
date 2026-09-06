using Wordle
using Test
using InlineStrings
using DataFrames

@testset "Wordle (Fidelity)                                                   " begin
    @test length(detect_ambiguities(Wordle)) == 0
end

@testset "Wordle (create_wordle_info)                                         " begin

    winfo, d = create_wordle_info(InlineString("which"), InlineString("where"))
    @test  winfo == [('w', 1), ('h', 2)]
    @test  d ==  Dict('h' => (0, 0), 'c' => (0, 0), 'i' => (0, 0))

    winfo, d = create_wordle_info(InlineString("teens"), InlineString("where"))
    @test  winfo == [('e', 3), ('e', -2)]
    @test  d == Dict('n' => (0, 0), 's' => (0, 0), 't' => (0, 0), 'e' => (1, 1))

    # Plain strings work; an exact match has no inexact info; lengths must agree.
    winfo, d = create_wordle_info("taste", "taste")
    @test winfo == [('t', 1), ('a', 2), ('s', 3), ('t', 4), ('e', 5)] && isempty(d)
    @test_throws BadLength create_wordle_info("abcdef", "abcde")
    @test_throws BadLength create_wordle_info("abcd", "abcde")
end

@testset "Wordle (filter_universe)                                            " begin
    ## Universe of words.
	words    = inlinestrings(["state", "which", "where", "child", "there", "taste"])

    winfo, d = create_wordle_info(InlineString("which"), InlineString("where"))
    filter_words = filter_universe((winfo, d), words)
	@test filter_words == InlineString["where"]

    # The puzzle word always survives its own filter; a wrong guess never does.
    for guess in words, pword in words
        fw = filter_universe(create_wordle_info(guess, pword), words)
        @test pword in fw
        @test guess == pword || !(guess in fw)
    end
    @test filter_universe((winfo, d), String7[]) == String7[]
end

@testset "Wordle (get_next_word / freq_letter_strat)                          " begin
    words = ["there", "ether", "three", "state", "taste"]
    wts   = [0.5, 0.1, 0.1, 0.2, 0.1]
    # "there"/"ether"/"three" share the letter group "eehrt" with total weight 0.7 > 0.3.
    @test get_next_word(words, wts) == 1
    @test get_next_word(words, [5, 1, 1, 2, 1]) == 1          # integer weights
    @test get_next_word(words, [0.1, 0.1, 0.1, 0.5, 0.4]) == 4 # "aestt" group wins with 0.9
    @test get_next_word(["alone"], [1.0]) == 1
    @test_throws ArgumentError get_next_word(String[], Float64[])
    @test_throws DimensionMismatch get_next_word(words, [1.0, 2.0])

    swords = ["taste", "waste", "paste", "state"]
    @test freq_letter_strat(swords, LFA, [1, 2]) in swords
    @test freq_letter_strat(swords, LFA, [1]) == "taste"       # 't' at index 1 is most frequent... after "taste"
    @test_throws ArgumentError freq_letter_strat(["Which"], LFA, [1])
end

@testset "Wordle (solve_wordle with InlineString Inputs)                      " begin
    res = solve_wordle("taste"; init_guess="their")
	@test res == (Any[(String7("their"), [('t', 1), ('e', -3)], 3591), 
		(String7("taken"), [('t', 1), ('a', 2), ('e', -4)], 34), 
		(String7("table"), [('t', 1), ('a', 2), ('e', 5)], 3), 
		(String7("taste"), [('t', 1), ('a', 2), ('s', 3),('t', 4), ('e', 5)], 2)], 4, :SUCCESS)

    # The guess-strategy path.
    res = solve_wordle("taste"; guess_strategy=freq_letter_strat, ul=1, uu=4000)
    @test res[3] == :SUCCESS && res[2] == 3
    @test [String(s[1]) for s in res[1]] == ["stare", "waste", "taste"]

    # A word that needs more than six guesses is a failure, even on the one-word-left shortcut.
    res = solve_wordle("stall")
    @test res[3] == :FAILURE && res[2] == 7

    # A word not in the universe.
    res = solve_wordle("zzzzz")
    @test res[3] == :FAILURE
end

@testset "Wordle (solve_wordle with other universes)                          " begin
    # Six letter words (plain Strings).
    df6 = DataFrame(word=["stares", "starts", "static", "stated"], freq=[0.4, 0.3, 0.2, 0.1])
    res = solve_wordle("stated", df6; init_guess="stares")
    @test res[3] == :SUCCESS && res[1][end][1] == "stated" || res[3] == :SUCCESS

    # Eight letter words (longer than String7).
    df8 = DataFrame(word=["absolute", "abstract", "academic", "accepted"], freq=[0.4, 0.3, 0.2, 0.1])
    res = solve_wordle("accepted", df8; init_guess="absolute")
    @test res[3] == :SUCCESS

    # Contract violations.
    mixed = DataFrame(word=["zzzzz", "bbbb", "abcde", "qqqqq"], freq=[0.5, 0.3, 0.2, 0.1])
    @test_throws DomainError solve_wordle("qqqqq", mixed)
    unsorted = DataFrame(word=["abcde", "zzzzz"], freq=[0.1, 0.5])
    @test_throws NotSorted solve_wordle("abcde", unsorted)
    @test_throws BadLength solve_wordle("abcd", df6)
    @test_throws BadLength solve_wordle("stated", df6; init_guess="abcd")
    @test_throws DomainError solve_wordle("abcde", DataFrame(word=["abcde"], freq=[1.0]))
    @test_throws DomainError solve_wordle("abcde", DataFrame(words=["abcde", "fghij"], freq=[1.0, 0.5]))
end

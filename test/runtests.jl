using Wordle
using Test
using InlineStrings

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
end

@testset "Wordle (filter_universe)                                            " begin
    ## Universe of words.
	words    = inlinestrings(["state", "which", "where", "child", "there", "taste"])

    winfo, d = create_wordle_info(InlineString("which"), InlineString("where"))
    filter_words = filter_universe((winfo, d), words)
	@test filter_words == InlineString["where"]
end

@testset "Wordle (solve_wordle with InlineString String/Inputs)             " begin
    res = solve_wordle("taste"; init_guess="their")
	@test res == (Any[(String7("their"), [('t', 1), ('e', -3)], 3585), 
					  (String7("taken"), [('t', 1), ('a', 2), ('e', -4)], 34), 
					  (String7("table"), [('t', 1), ('a', 2), ('e', 5)], 3), 
					  (String7("taste"), [('t', 1), ('a', 2), ('s', 3), ('t', 4), ('e', 5)], 2)], 4, :SUCCESS)

end





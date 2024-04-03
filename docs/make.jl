using Wordle
import Pkg

Pkg.add("Documenter")
using Documenter

makedocs(
	sitename = "Wordle",
	format = Documenter.HTML(),
	modules = [Wordle]
	)

	# Documenter can also automatically deploy documentation to gh-pages.
	# See "Hosting Documentation" and deploydocs() in the Documenter manual
	# for more information.
	deploydocs(
		repo = "github.com/scottrsm/Wordle.jl.git"
	)

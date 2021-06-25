.PHONY: hoogle build repl test watch test-watch requires_nix_shell fourmolu-format

hoogle: requires_nix_shell
	hoogle server --local --port 8008

build: 
	stack build --ghc-options="-Wall"

repl: 
	stack ghci

test: 
	stack test all 

watch: 
	stack build --file-watch --ghc-options="-Wall"

test-watch: 
	stack test --file-watch 

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ "($IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "($IN_NIX_SHELL)" ] || (echo "    run 'nix-shell --pure' first" && false)

# Format the haskell files as per Specifications.md.
# Add folder locations to the list to be reformatted. 
fourmolu-format:
	@ echo "> Formatting all .hs files"
	fourmolu -i $$(find src/  -iregex ".*.hs")
	fourmolu -i $$(find test/ -iregex ".*.hs")
	fourmolu -i $$(find app/  -iregex ".*.hs")



.PHONY: build-nix hoogle build nix-build-library nix-build-executables \
        nix-build-test nix-repl requires_nix_shell ci-build-run

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate_readme_contents:
	nix-shell -p nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Starts a hoogle Server.
hoogle: requires_nix_shell
	@ nix-shell --pure --command "hoogle server --local --port 8008"

# Attempt the CI locally
ci-build-run:
	@ nix-build ./nix/ci.nix  

# Build the library with nix.
nix-build-library:
	@ nix-build -A mlabs-plutus-use-cases.components

# Build the executables with nix.
nix-build-executables:
	@ nix-build -A mlabs-plutus-use-cases.components.exes

# Build the tests with nix.
nix-build-test:
	@ nix-build -A mlabs-plutus-use-cases.components.tests

# Starts a ghci repl inside the nix environment.
nix-repl: 
	@ nix-shell --pure --command "cabal new-repl"

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "($IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ "($IN_NIX_SHELL)" ] || (echo "    run 'nix-shell --pure' first" && false)

# Build with Stack - independent of NIX.
stack-build: 
	stack build --ghc-options="-Wall"

# Test with Stack.
stack-test: 
	stack test all 

# Watch with Stack.
stack-watch: 
	stack build --file-watch --ghc-options="-Wall"

# Watch Test with Stack.
stack-test-watch: 
	stack test --file-watch 
	
# Add folder locations to the list to be reformatted. 
fourmolu-format:
	@ echo "> Formatting all .hs files"
	fourmolu -i $$(find src/  -iregex ".*.hs")
	fourmolu -i $$(find test/ -iregex ".*.hs")
	fourmolu -i $$(find app/  -iregex ".*.hs")


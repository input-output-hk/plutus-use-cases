.PHONY: build-nix hoogle nix-build-library nix-build-executables \
        nix-build-test nix-cabal-repl requires_nix_shell ci-build-run

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate-readme-contents:
	nix shell nixpkgs#nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Starts a hoogle Server.
hoogle:
	@ nix develop -c hoogle server --local --port 8008

# Attempt the CI locally
ci-build-run:
	@ ./run-tests.sh

# Build the library with nix.
nix-build-library:
	@ nix build .#mlabs-plutus-use-cases:lib:mlabs-plutus-use-cases

current-system := $(shell nix eval --impure --expr builtins.currentSystem)

# Build the executables with nix (also builds the test suite).
nix-build-executables:
	@ nix build .#check.${current-system}

# Build the tests with nix.
nix-build-test:
	@ nix build .#mlabs-plutus-use-cases:test:mlabs-plutus-use-cases-tests

# Starts a ghci repl inside the nix environment.
nix-cabal-repl:
	@ nix develop -c cabal new-repl

# Target to use as dependency to fail if not inside nix-shell.
requires_nix_shell:
	@ [ "($IN_NIX_SHELL)" ] || echo "The $(MAKECMDGOALS) target must be run from inside `nix develop`"
	@ [ "($IN_NIX_SHELL)" ] || (echo "    run `nix develop` first" && false)

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

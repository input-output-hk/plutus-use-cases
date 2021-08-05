PROJ = "mlabs-plutus-use-cases"   # Project Name 
COMP = "$(PROJ).components"       # Libraries
EXES = "$(PROJ).components.exes"  # Executables 
TEST = "$(PROJ).components.tests" # Tests
 
.PHONY: build-nix hoogle build nix-build-library nix-build-executables \
        nix-build-test nix-repl requires_nix_shell


# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
generate_readme_contents:
	nix-shell -p nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"

# Starts a hoogle Server.
hoogle: requires_nix_shell
	@ nix-shell --pure --command "hoogle server --local --port 8008"

# Build the library with nix.
nix-build-library:
	@ nix-build -A $(COMP)

# Build the executables with nix.
nix-build-executables:
	@ nix-build -A $(EXES)

# Build the tests with nix.
nix-build-test:
	@ nix-build -A $(TEST)

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

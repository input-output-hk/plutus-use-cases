
hoogle: requires_nix_shell
	hoogle server --local --port 8008

build: 
	stack build --ghc-options="-Wall"

# Not working ATM
build-nix:
	nix-build -A mlabs-plutus-use-cases --show-trace

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

# Generate TOC for README.md
# It has to be manually inserted into the README.md for now.
readme_contents:
	nix-shell -p nodePackages.npm --command "npx markdown-toc ./README.md --no-firsth1"


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
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix-shell --pure' first" && false)

exec-lendex-demo:
	stack build --no-nix --system-ghc --exec lendex-demo

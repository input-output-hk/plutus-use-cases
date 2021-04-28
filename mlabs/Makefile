
hoogle: requires_nix_shell
	hoogle server --local --port 8008

build: requires_nix_shell
	cabal build all

repl: requires_nix_shell
	cabal new-repl

test: requires_nix_shell
	cabal test all

watch: requires_nix_shell
	ghcid "-c cabal new-repl"

# Target to use as dependency to fail if not inside nix-shell
requires_nix_shell:
	@ [ -v IN_NIX_SHELL ] || echo "The $(MAKECMDGOALS) target must be run from inside nix-shell"
	@ [ -v IN_NIX_SHELL ] || (echo "    run 'nix-shell --pure' first" && false)


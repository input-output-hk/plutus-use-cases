# Nix tools for Mlabs Plutus Use Cases

This directory contains all of the nix helper functions used to build our
dependencies.

# Formatting

Use nixfmt (provided by the shell) to format the nix sources.

# Pinning git dependencies

Git dependencies are pinned in the `inputs` of the flake. Make sure to set `flake = false;` when adding a new dependencies. When upgrading an existing dependency, replace the commit hash in its `url`.

# Using flakes commands

This repository recently switched to flakes, a fairly new (and still experimental)
feature of the nix package manager. Flakes bring a lot of improvements to nix,
including evaluation caching and greater consistency and reproducibility.

This section is intended to help you transition to using flakes, illustrating flake
equivalents of old `nix-*` commands.

**Note**: You will need Nix version 2.4 or greater to use the new `nix` command
and subcommands

**Note**: Due to the use of IFD ("import from derivation") in haskell.nix, `nix flake show`
and `nix flake check` do not currently work.

## `nix-shell`

Use `nix develop`

## `nix-build`

Use `nix build`

### Building project components

Previously, to build specific project components, `nix-build -A mlabs-plutus-use-cases.components.*`
could be used. Project components can now be identified using the flake selector `#` followed by
cabal-like syntax.

For example, to build the executable `lendex-demo`:

Old:

`nix-build -A mlabs-plutus-use-cases.components.exes.lendex-demo`

New:

`nix build .#mlabs-plutus-use-cases:exe:deploy-app`

### Build all derivations that will be built in CI

`nix build .#check.<SYSTEM>` builds all of the project packages and runs the tests.

## More helpful commands

See all of the flake outputs: `nix flake show`
See flake metadata, including inputs: `nix flake metadata`

## Compatibility

In case you cannot upgrade to Nix 2.4 or prefer the older interface, compatibility `shell.nix` and
`default.nix` remain in this repository.

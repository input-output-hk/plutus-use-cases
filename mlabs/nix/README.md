# Nix tools for Mlabs Plutus Use Cases

This directory contains all of the nix helper functions used to build our
dependencies.

# Formatting

Use nixfmt (provided by the shell) to format the nix sources.

# Pinning git dependencies

Use `niv` to update the git dependencies in `cabal.project`.

- to update a pinned dependency:

```shell
niv update <dependency_name> -r <dependency_tag>
```

This will update both the revision, and the sha256 of the said dependency, that
will then get pulled by haskell-nix.

To update all of the dependencies with `niv`, run the `update-sha256map.sh` script
in the repository root.

# Updating plutus

In the case of a `plutus` upgrade, you _must_ also update the `rev` field of `plutusSrc`
in `flake.nix` in addition to the steps above:

```shell
niv update plutus -r <revision>
```

then

```nix
# ../flake.nix
{
  inputs = {

    plutusSrc = {
      type = "github";
      owner = "input-output-hk";
      repo = "plutus";
      rev = "3f089ccf0ca746b399c99afe51e063b0640af547"; # update here!
      flake = false;
    };

  }

}
```

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

(See note about IFD problem above).

As currently configured, `nix flake check` will build all project components, including the tests
and executables. If the `-L` flag is included, the build logs will be fully printed to stdout.

You can also run the `run-tests.sh` script in the repository root which will build all project
components.

## More helpful commands

See all of the flake outputs: `nix flake show`
See flake metadata, including inputs: `nix flake metadata`

## Compatibility

In case you cannot upgrade to Nix 2.4 or prefer the older interface, compatibility `shell.nix` and
`default.nix` remain in this repository.

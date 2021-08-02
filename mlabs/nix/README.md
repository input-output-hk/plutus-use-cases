# Nix tools for Mlabs Plutus Use Cases

This directory contains all of the nix helper functions used to build our
dependencies.

# Formatting

Use nixfmt (provided by the shell) to format the nix sources.

# Niv dependency pinning

Use `niv` to update / modify nix dependencies.

# Updating plutus

In order to update the plutus revision, a few steps must be taken:

- `niv update plutus -r <revision>` will update Nix's plutus revision which will
  also produce a new shell

- You should update non-plutus entries in the sha256map in `./nix/haskell.nix`.
  You can copy from the plutus repo
  [here](https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix)

- Update the revision in `cabal.project` and paste the plutus repo's
  `cabal.project` contents after the `*replace here*` separator

Now everything should be updated, good luck fixing compile errors!

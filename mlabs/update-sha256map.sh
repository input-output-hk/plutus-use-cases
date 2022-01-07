#!/usr/bin/env bash

nix-instantiate nix/update.nix
nix-instantiate nix/update.nix --eval --strict --json \
    | nix shell nixpkgs#jq -c jq -r \
    '.[] | "nix shell nixpkgs#niv -c niv update \(.name) -r \(.tag)"' \
    | bash -x

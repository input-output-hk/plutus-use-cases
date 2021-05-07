# Plutus Platform starter project

This project gives a simple starter project for using the Plutus Platform.

## Setting up

- Install nix
- Clone https://github.com/input-output-hk/plutus
- Set up your machine to build things with Nix, following the Plutus README (make sure to set up the binary cache!)

## The Plutus Application Backend (PAB) example

We have provided an example PAB application in `./pab`. With the PAB we can serve and interact
with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

1. Enter the nix shell (cd to the cloned Plutus repo):

```
git checkout 58bf9ed626d498c140c69a859a508da03843d097
nix-shell
```

2. Build the PAB executable (cd to plutus-use-cases/MetaLamp/lending-pool):

```
cabal build
```

3. Run the PAB binary:

```
cabal run plutus-starter-pab
````

This will then start up the server on port 8080.

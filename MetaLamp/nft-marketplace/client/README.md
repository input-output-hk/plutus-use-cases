# Lending pool client

The client application has a minimalistic interface to the PAB [server](../README.md).

## Running the project

1. Enter the nix shell (from `nft-marketplace` directory):

```
nix-shell
```

Cd to `./client` folder.


2. Install npm packages.

```
npm install
```

3. Generate necessary PureScript code from Haskell source. This step runs an executable(`generate-purs`) from `nft-marketplace` directory, which requires a certain environment. The setup steps are described in `nft-marketplace/README`. Provided that you are able to build the backend, you can use the same approach to run purescript generation from `client` folder, i.e.


```
npm run generate-purs
```

4. Start the client:

```
npm start
```

5. Open browser to interact with the app at https://localhost:8009/.
## Troubleshooting

Sometimes the build results in error with Haskell IDE enabled. If the build does not work or the app behaves strangely, disable IDE and clean all source files:

```
cd MetaLamp/nft-marketplace/ && cabal clean
```

```
`cd MetaLamp/nft-marketplace/client && rm -rf node_modules/ generated/ output/ plutus-purs/ .spago/`
```

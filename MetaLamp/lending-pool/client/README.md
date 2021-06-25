# Lending pool client

The client application has a minimalistic interface to the PAB [server](/MetaLamp/lending-pool/README.md).

## Running the project

1. Install npm packages.

```
npm install
```

2. Generate necessary PureScript code from Haskell source. This step runs an executable(`generate-purs`) from `lending-pool` directory, which requires a certain environment. The setup steps are described in `lending-pool/README`. Provided that you are able to build the backend, you can use the same approach to run purescript generation from `client` folder, i.e.

1. Enter the nix shell (from `MetaLamp/lending-pool`):

```
nix-shell
```

cd to `lending-pool/client` folder and execute

```
npm run generate-purs
```

3. Start the client:

```
npm start
```

4. Open browser to interact with the app at https://localhost:8009/.
CORS protection needs to be disabled. You can use this script to launch chromium (note that first you need to close chromium completely, otherwise security won't be disabled):

```
npm run start-chrome
```

## Troubleshooting

Sometimes the build results in error with Haskell IDE enabled. If the build does not work or the app behaves strangely, disable IDE and clean all source files:

```
cd MetaLamp/lending-pool/ && cabal clean
```

```
cd MetaLamp/lending-pool/client && rm -rf node_modules/ generated/ output/ plutus-purs/ .spago/
```

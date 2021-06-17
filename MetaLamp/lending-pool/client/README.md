# Lending pool client

The client application has minimalistic interface to the PAB [server](/MetaLamp/lending-pool/README.md).

## Running the project

1. Generate necessary PureScript code from Haskell source (cd to /MetaLamp/lending-pool/client/):

```
npm install
```

2. Start the client:

```
npm start
```

3. Open browser to interact with the app at https://localhost:8009/.
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

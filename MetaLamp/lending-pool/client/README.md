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

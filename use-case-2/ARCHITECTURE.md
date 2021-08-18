##Architecture

This document is in place to give the developer a high level understanding of how this app's structural design. In this document you will find information about request/response handling, smart contract interaction, smart contract and transaction fee estimation, relational database schema layout. 

### Request Handlers

Requests that have the intention of communicating with the smart contracts are made on the frontend and sent to the backend of the application where an HTTP request is sent to Plutus Application Backend(PAB) that simulates communication with a deployed smart contract. A list of API requests can be found within `common/src/Common/Api.hs`. Note: Some familiarity on how to read Haskell function headers will immediately provide you with an idea of the input and output to these application APIs. 

### Request Responses

Responses to requests that have been made are not returned from the http request made on the backend of the application. Instead, the responses for the request can be discovered in two ways. 

One way is to submit an empty request to the `http://localhost:8080/api/new/contract/instances` endpoint and parse it's response for the `observableState` JSON key. Within these `observableState` JSON objects, there will be another key called `tag` provide the subject matter of the state being observed. For example, if awaiting the results of a Swap requests the value of `tag` will be `Swapped`.

The second way is to listen for incoming smart contract `observableState` via websocket. Connection with the PAB websocket is established in this case using `ws://localhost:8080/ws/[contract-instance-id]`. In the frontend, Reflex-FRP's websocket libraries are used to detect changes in websocket data to generate events to provoke updates within the DOM. This is how it is possible to know when a swap was successful, when to update the wallet balances, fetch, and perform other calculations based on incoming response data.

### Estimations

Swap, Stake, and Remove Liquidity estimations are accomplished using a combination of information exposed by the pools and funds and pure functions derived from Uniswap v2 algorithms (see `common/src/Common/Plutus/Contracts/Uniswap/Estimates.hs`).

Transaction fees are derived using regression analytical algorithms on a history of smart contract transactions that have been persisted to the application's backend database. At the time of writing this application there was no software or tooling in place that reliably estimated transaction fees. This is largely an experimental feature to encourage the application not to be fully reliant on what PAB may or may not provide to estimate transactions. Profound predictors, so far, that have the most impact on the outcome of the transaction fee are the script size, and the amount of time it takes for the chain to process the script. This feature will be updated to meet PAB best practices as internal development continues.

### Database

There are 3 tables persisting information in this application.
1. db_contract - The contract id of available wallets that can interact with the Uniswap smart contract. This table is used to display the available wallets a user can perform smart contract actions with on the landing page of the application.
2. db_pooledTokens - The available tokens with supportive pools that enable them to be swappable. This table is used to display tokens that can be swapped in dropdown menus.
3. db_txFeeDataSet - The history of smart contract transactions. This table is used to assemble a data set of uniswap transactions that is later referenced during transaction regression estimation. 

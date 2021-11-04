# Plutus Use Cases

## Welcome to the Plutus Partners developers program!

The aim of this repository is to synthetize work of all partners on use cases Plutus Partner program regarding the showcases.

### Use cases

These are the following use cases that partners should pick &amp; work on:

 ### Use case 1: Oracle

Create a fully functional POC Oracle to bring off chain data on chain to interact and feed smart contracts, In addition, create a reference implementation of a centralized trusted off-chain data feed for Plutus Applications (e.g. interacting price feeds from various centralized exchanges)

Chain link contracts reference:

[https://github.com/smartcontractkit/chainlink/tree/develop/contracts/src/v0.8](https://github.com/smartcontractkit/chainlink/tree/develop/contracts/src/v0.8)

 ### Use case 2: DEX Token Swap, Liquidity and Stake Pools

Create a full POC that allows users to swap between supported tokens within one smart contract method call. Users can also contribute to liquidity pools for any supported token, and therefore gain commissions in the form of exchange fees for doing so. When liquidity is provided to a pool, the user receives a liquidity token representing that deposit. The contract should calculate fees (e.g. 0.3%) which are then dispersed to liquidity providers dependent on each provider&#39;s share of liquidity pool.

PancakeSwap Core contracts for reference:

[https://github.com/pancakeswap/pancake-farm/tree/master/contracts](https://github.com/pancakeswap/pancake-farm/tree/master/contracts)

- Additional libraries: Wrapped token, voting, etc.:

[https://github.com/pancakeswap/pancake-farm/tree/master/contracts/libs](https://github.com/pancakeswap/pancake-farm/tree/master/contracts/libs)

### Use case 3: Lending and borrowing, collateral escrow, flashloans

Create POC for a lending protocol such as Aave that enables users to lend and borrow cryptocurrencies of their choice in a trustworthy way while offering stable and variable interest rates. Users can participate as depositors or borrowers, and to transact lenders have to deposit their funds into liquidity pools, and borrowers can borrow from such liquidity pools. Depositors receive interest-bearing tokens (known as aTokens) in return. Each pool sets aside as reserves to safeguard against volatility.

Bonus features:

- Algorithmically adjust the interest rate on the basis of demand and supply
- Flash loans that settle within a single block

Aave v2 contracts for reference:

[https://github.com/aave/protocol-v2/tree/master/contracts](https://github.com/aave/protocol-v2/tree/master/contracts)

### Use case 4: Crypto backed stable coin with staking

Create new stable coin implementation based on chain collateral using Atala identity system on Cardano blockchain. Should be a full reference implementation that includes transfer restrictions and asset freezing etc.

PAX contracts for reference: [https://github.com/paxosglobal/pax-contracts/tree/master/contracts](https://github.com/paxosglobal/pax-contracts/tree/master/contracts)

### Use case 5: NFTs. Minting, transfer, buying and selling NFTs

The core functionality is minting, sending, receiving NFTs into a wallet, but the core scenarios are open.

Crypto Heroes (ERC 721) for reference:

[https://github.com/PortalNetwork/nifty-game/tree/develop/contracts](https://github.com/PortalNetwork/nifty-game/tree/develop/contracts)

OpenSea marketplace (ERC 1155) for reference:

[https://github.com/ProjectOpenSea/opensea-erc1155/tree/master/contracts](https://github.com/ProjectOpenSea/opensea-erc1155/tree/master/contracts)

### Use case 6: DeFi Tools, e.g. ZapperFi and Zerion multipurpose dashboards

Today there are currently various products, both web based and mobile, that integrate with smart contracts to bring value to native tokens traders. These products have multiple functional dashboards, to show balances of tokens and liquidity pools, etc. They also bundle together multiple functions such as swapping and providing liquidity into a single transaction making DeFi adoption easier.

### Repository file structure description

Each partner company should create its own file in repository and name it by the company name. Then, inside that folder create separate folder for each chosen use case. In case that partner have chosen more than one use case to work on, then create subfolder for each use case separately.

Folder structure should be like this:

 - [partner] folder
   - [Use case 1] subfolder
   - [Use case 2] subfolder

Code base inside the use case folders should be well commented, for easier understanding and effective providing feedback.


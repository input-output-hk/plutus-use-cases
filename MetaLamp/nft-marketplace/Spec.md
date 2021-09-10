# NFT Marketplace

## Brief description

NFT Marketplace Service provides an ability to create NFT tokens for any file and put them up to for sale or auction. Marketplace allows combining NFT tokens into the bundle and operate them as a single unit when sell or buy it.

## Monetization

The marketplace provider receives a tip from NFT selling and auctioning.

## Glossary

**Bundle** - a collection of tokens provided as a single unit.

**Sale** - a protocol when NFT owner sets a fix price for NFT and opens sale for NFT buyers. NFT seller can cancel the sale if NFT isn't bought at that moment. Works for bundles as well.

**Auction** - a protocol when NFT owner puts up an NFT for auction by 0 Ada, and sets a timeout of auction's duration. NFT bidders can bid their price before the timeout. The winner is the bidder who made a last bid. If there are no bids on timeout, NFT returns to its seller. Works for bundles as well.

**Marketplace tips** - a marketplace provider profit by carrying auctions and sales.

## Users categories

**Marketplace provider** - an actor who started a marketplace smart contract

**NFT owner** - an actor who owns an NFT token

**NFT seller** - an NFT owner who put his NFT up to the sale or auction

**NFT buyer/NFT bidder** - an actor who try to buy an NFT on the Sale/make a bid on the Auction

## System architecture

![alt tag](readme-src/NFTMarketplaceArchitecture.png)

## Example of the flow

![alt tag](readme-src/NFTMarketplaceFlow.png)

## Features list

### Marketplace

`start()` - Start a marketplace smart contract.

### Marketplace user

`createNft()` - Mint NFT token and add it to the marketplace.

`addNft()` - Add NFT from another marketplace, which use the same minting protocol ([IPFS cId as a token name](#implementation-features)).

`openSale()` - Opens sale for NFT.

`buyItem()` - Buy NFT.

`closeSale()` - Close sale and receive the token back.

`startAnAuction()` - Start an auction for specified NFT.

`completeAnAuction()` - Complete auction before the timeout.

`bundleUp()` - Create a bundle from specified NFTs.

`unbundle()` - Unbundle specified NFTs.

`ownPubKey()` - Get `pubKeyHash` for public key belonging to the wallet of a marketplace provider.

`ownPubKeyBalance()` - Get balance on marketplace provider address.

### Marketplace info

`fundsAt()` - Get all UTxOs belonging to a user.

`marketplaceFunds()` - Get all UTxOs belonging to the Marketplace.

`marketplaceStore()` - Get current marketplace store state.

`getAuctionState()` - Get current auction state for specified NFT

## Implementation features

The main features are related to uploading a file and minting NFT. 

- **Uniqueness of an uploaded file:** Using an IPFS - [Content-addressable Storage](https://en.wikipedia.org/wiki/Content-addressable_storage) to store uploading files, that guarantee the uniqueness of a linked file. `IPFS cId` link is used to link the NFT with file in a storage.

- **Availability of cId:** `CId` is available in NFT owner's wallet - it is used as a token name. Using a `cId` as a token name is a requirement for this marketplace implementation. NFT can be imported from another marketplace, only if that requirement was abided.

- **Privacy of cId link:** Using `cId` hash for the token name in on-chain code, to hide the NFT token until NFT owner decide to make it public putting it up on the sale or auction.

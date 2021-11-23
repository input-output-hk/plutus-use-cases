# Endpoints Documentation

This document describes endpoints available in NFT markterplace application.

## Admin endpoints

### App Init (`app-init`)

prerequisite:
- none

input: none

behaviour: Starts NFT marketplace application, mintes HEAD and unique token.

## User endpoints

### Mint (`mint`)

prerequisite:
- App is initialised
- NFT was not minted before

input: Mlabs.NFT.Types.MintParams

behaviour: Mints new NFT. If the price is `Nothing` then the NFT is not for sale
and the owner must call `set-price` to allow sales.

### Set price (`set-price`)

prerequisite:
- App is initialised
- User must be current owner
- NFT is not on auction

input: Mlabs.NFT.Types.SetPriceParams

behaviour: updates the `info'price` parameter

### Buy (`buy`)

prerequisite:
- App is initialised
- User must have necessary ADA in wallet
- `info'price` parameter is not `Nothing`

input: Mlabs.NFT.Types.BuyRequestUser

behaviour:

If the `BuyRequestUser.ur'price` is greater than or equal to the asking price,
the user's wallet will be reduced by Buy.Price ADA (the contract must fail if
the user has less than the specified Buy.price) the funds sent by the caller
('the buyer') are split such that (`share` * `price` parameter amount) is sent
to the author, and the remainder is sent to the current owner.

For example, if the author set a share to 1/10, and the buyer paid 100 ADA, the
author would receive 10 ADA and the owner would receive the rest. The owner is
set to the caller if the above is successful the asking price is set to the
`BuyRequestUser.ur'newPrice`.

### Auction open (`auction-open`)

prerequisite:
- App is initialised
- User must be current owner
- NFT is not on auction
- `as'minBid` is greater or equal to 2 ADA

input: Mlabs.NFT.Types.AuctionOpenParams

behaviour:

Sets the `info'price` parameter to `Nothing` (NFT is no longer for sale), and sets `info'auctionState` to `Just` starting an auction.

### Auction bid (`auction-bid`)

prerequisite:
- App is initialised
- NFT is on auction
- Bid (`bp'bidAmount`) is higher than `as'minBid`
- Bid is higher than `as'highesBid`, when `as'highesBid` is `Just`
- `as'deadline` is not reached

input: Mlabs.NFT.Types.AuctionBidParams

behaviour:
Bid amount is lock in the script, previous bid is sent back, updates `as'highesBid`

### Auction close (`auction-close`)

prerequisite:
- App is initialised
- NFT is on auction
- User is auction winner

input: Mlabs.NFT.Types.AuctionCloseParams

behaviour: NFT is sent to user, Highest bid is unlocked from script and paid to
previous owner and author, as described in `buy` endpoint.

## Query endpoints

### Query Current Price (`query-current-owner`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.NftId

behaviour: Returns current price of NFT.

### Query Current Owner (`query-current-price`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.NftId

behaviour: Returns current owner of NFT.

### Query List Nfts (`query-list-nfts`)

prerequisite:
- App is initialised

input: None

behaviour: Returns list of all NFTs available in the app.

### Query Content (`query-content`)

prerequisite:
- App is initialised

input: Mlabs.NFT.Types.Content

behaviour: Returns status of NFT given content.

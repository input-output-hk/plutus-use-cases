# NFT Contract Specification

This project adapts the Ethereum-style approach to NFTs as a digital certificate
of authenticity or ownership, it allows a creator to make a digital asset
representing some artwork, then sell the asset, and for owners of the asset to
be confirmed.

ownership can only be transferred through the contract, so when the asset is
re-sold, a royalty to the artist can be enforced

## Author Contract

### StartParams

prerequisite: none

input:
Mlabs.Nft.Contract.Api.StartParams
(content, share, price)

behavior: instantiates the 'User' Contract, which represents an asset described
by `content` the author is set to the original owner the entire `StartParams`
will need to be kept as some internal state to be referenced/updated, along with
the author and the current owner

if the price is Nothing - then the NFT is not for sale and the user must call
`SetPrice` to allow sales.

## User Contract

All endpoints on this contract presume that AuthorContract.StartParams has been
called. 

### SetPrice

Prerequiste: none beyond contract instantiation
must be the current owner

input:
Mlabs.Nft.Contract.Api.SetPrice

behavior:  
updates the `price` parameter needed for the `Buy` endpoint

### Buy

prerequisite: user must have the necessary ada in their wallet the current
asking price specified by a call to either `StartParams` or `SetPrice` must be a
Just.   if it is a Nothing, then the asset is not for sale.

input:
Mlabs.Nft.Contract.Api.Buy
(price, newprice)

behavior:

if the Buy.price is greater than or equal to the asking price, the user's wallet
will be reduced by Buy.Price Ada (the contract must fail if the user has less
than the specified Buy.price) the funds sent by the caller ('the buyer') are
split such that (`share` * `price` parameter amount) is sent to the author, and
the remainder is sent to the current owner.

for example, if the author set a share to 1/10, and the buyer paid 100 ada,  the
authoer would receive 10 ada and the owner would receive the rest. the owner is
set to the caller if the above is successful the asking price is set to the
Buy.newprice.

### QueryCurrentOwner

Returns the address of the current owner.

### QueryCurrentPrice

Returns the current `price` parameter so that a potential buyer can purchase the
item.

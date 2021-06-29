# MLabs: Plutus Use Cases

--------------------------------------------------------------------------------

## Contents

- [MLabs: Plutus Use Cases](#mlabs-plutus-use-cases)
  - [Contents](#contents)
  - [Overview](#overview)
    - [Prerequisites](#prerequisites)
    - [Building, Testing, Use](#building-testing-use)
      - [On Unix Systems](#on-unix-systems)
    - [Documentation](#documentation)
    - [Testing](#testing)
      - [Running Tests](#running-tests)
  - [Use Case: Lendex](#use-case-lendex)
    - [Lendex: Description](#lendex-description)
    - [Lendex: Progress & Planning](#lendex-progress--planning)
    - [Lendex: Examples](#lendex-examples)
    - [Lendex: APIs & Endpoints](#lendex-apis--endpoints)
    - [Lendex: Tests](#lendex-tests)
    - [Lendex: Notes](#lendex-notes)
  - [Use Case: NFT](#use-case-nft)
    - [NFT: Description](#nft-description)
    - [NFT: Progress & Planning](#nft-progress--planning)
    - [NFT: Examples](#nft-examples)
    - [NFT: APIs & Endpoints](#nft-apis--endpoints)
    - [NFT: Tests](#nft-tests)
    - [NFT: Notes](#nft-notes)

*note: the table of contents is generated using `make readme_contents`, please
update as headings are expanded.*

--------------------------------------------------------------------------------

## Overview

MLabs has been working on developing two Plutus Use cases, specifically:

- [Use Case: Lendex](#use-case-lendex) based on the specification of 
  [Plutus Use case 3](https://github.com/mlabs-haskell/plutus-use-cases/tree/documentation#use-case-3-lending-and-borrowing-collateral-escrow-flashloans).

- [Use Case: NFT](#use-case-nft) based on the specification of 
  [Plutus Use case 5](https://github.com/mlabs-haskell/plutus-use-cases/tree/documentation#use-case-5-nfts-minting-transfer-buying-and-selling-nfts).

Please refer to each individual Plutus Use Case for more specific information.

### Prerequisites

- Git
- Curl
- Nix

### Building, Testing, Use

# HLS setup (tested for Visual Studio Code)
Start editor from nix-shell. Let the editor find the correct version of haskell-language-server binary.
#### On Unix Systems

*It is recommended that all current updates to your system be done before
installation*

1) ***Install basic dependencies***

```bash
sudo apt install curl
sudo apt install git
```

2) ***Clone Directory***

Create a directory and clone the project:

```bash
git clone https://github.com/mlabs-haskell/plutus-use-cases.git
```

3) ***Install Nix***
  
  1) **Setup Nix**

  ```bash
  $ curl -L https://nixos.org/nix/install | sh
  ```
    - *There is a issue with nix correctly adjusting the PATH in some machines. 
      Please re-start your terminal and make sure Nix is in the path (`nix --version`).
      See this discussion if you are having this issue: [https://github.com/NixOS/nix/issues/3317](https://github.com/NixOS/nix/issues/3317).*

    - *The direct link to the nix download page for reference: [https://nixos.org/download.html](https://nixos.org/download.html).*

   2) **Set up binary cache** 
   
   **note: Make sure to set up the IOHK binary cache. If you do not do this, you
            will end up building GHC, which takes several hours. If you find
            yourself building GHC, STOP and fix the cache.**
	
    - To set up the binary cache:
		
    * On **non-NixOS** machines: 
  
  Create a nix directory and file in the `etc` directory.
			
       1) `sudo mkdir /etc/nix`
			
       2) `sudo touch /etc/nix/nix.conf`
			
     *Then edit your `nix.conf` file to add:*
		
         `substituters        = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/`
         `trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=`
     
     
  * On **NixOS** Machines, add the following NixOs options:
         
         `nix = {
          binaryCaches          = [ "https://hydra.iohk.io" "https://iohk.cachix.org" ];`
         `binaryCachePublicKeys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=" ];
          };`

Please see the original documentation at IOHK for reference: 
- [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus/blob/master/README.adoc#iohk-binary-cache)

4) ***Create nix shell***

Go to the `plutus-use-cases/mlabs` directory run the `nix-shell` command:
```bash
	$ nix-shell
```
- *note: This will take some time on the first run, as the dependencies get built locally.*

### Documentation
Currently the documentation is done via this document which can 
be found in the [MLabs gitHub repository](https://github.com/mlabs-haskell/plutus-use-cases/tree/main/mlabs)

### Testing
For an overview of the test coverage and implementation refer to the individual
cases documentation and the [test folder](https://github.com/mlabs-haskell/plutus-use-cases/tree/main/mlabs/test).

#### Running Tests
*TODO: Add the explanation of how to run tests*

--------------------------------------------------------------------------------

## Use Case: Lendex

### Lendex: Description

The Lendex Use Case is based on the Open Source, Non-Custodial Aave Protocol,
described in the [Aave Protocol
Whitepaper](https://github.com/aave/aave-protocol/blob/master/docs/Aave_Protocol_Whitepaper_v1_0.pdf).
The use case can be summarised as a platform for a decentralised, pool-based,
loan strategy.

As described in the whitepaper, the model relies on Lenders depositing (Cardano)
cryptocurrency in a Pool Contract. The same Pool Contract provides a source for
funds to be borrowed by Borrowers through the placement of a collateral. Loans do
not need to be individually matched, but rather rely on the pooled funds, the
amounts borrowed and their respective collateral. The model enables instant
loans and the interest rate for both borrowers and lenders is decided
algorithmically. A general description of the interest algorithm is:

- Borrower's interest is tied to the amount of funds available in the pool at
  a specific time - with scarcity of funds driving the interest rate up.
- Lender's interest rate corresponds to the earn rate, with the algorithm
  safeguarding a liquidity reserve to guarantee ease of withdrawals at any
  given time.

### Lendex: Progress & Planning

- Goals and status:
  - Development
    - [x] Feature Completeness as per Specifications
    - [ ] Improve Deployment Story
    - [ ] Improve Performance
    - [ ] Improve Ergonomics of Use and Installation

  - Testing
    - [x] 100% Test Coverage
    - [ ] QuickCheck Testing

  - Documentation
    - [x] Example
    - [ ] APIs

### Lendex: Examples

- [Lendex Demo](https://github.com/mlabs-haskell/plutus-use-cases/blob/main/mlabs/lendex-demo/Main.hs)
  - to run the `lendex-demo` run the following command from the root folder:
  
  ```bash
  cd mlabs \
  && nix-shell --command "cabal v2-repl lendex-demo"
  ```

Are defined in [mlabs/src/Mlabs/Lending/Contract/Api.hs](https://github.com/mlabs-haskell/plutus-use-cases/blob/main/mlabs/src/Mlabs/Lending/Contract/Api.hs#L146)

### Lendex: APIs & Endpoints

- User Actions
  
  - Deposit
    - [x] in use.
    - Description: *Deposit funds to app.*
  
  - Borrow
    - [x] in use.
    - Description: *Borrow funds by depositing a collateral.*
  
  - Repay
    - [x] in use.
    - Description: *Repay part of a Loan.*
  
  - SwapBorrowRateModel
    - [x] in use.
    - Description: *Swap borrow interest rate strategy (stable to variable).*
  
  - SetUserReserveAsCollateral
    - [x] in use.
    - Description: *Set some portion of deposit as collateral or some portion of collateral as deposit.*
  
  - Withdraw
    - [x] in use.
    - Description: *Withdraw funds from deposit.*
  
  - LiquidationCall
    - [x] in use.
    - Description: *Call to liquidate borrows that are unsafe due to health check. For further see [docs.aave.com/faq/liquidations](https://docs.aave.com/faq/liquidations)*

- Admin actions

  - AddReserve
    - [x] in use.
    - Description: *Adds a new reserve.*
  
  - StartParams
    - [x] in use.
    - Description: *Sets the start parameters for the Lendex*.

### Lendex: Tests

- To run the tests:

```bash
stack test all  
```

- To see test cases refer to: `./test/Test/Lending`

### Lendex: Notes

--------------------------------------------------------------------------------

## Use Case: NFT

### NFT: Description

The core functionality of the Non Fungible Tokens(i.e. NFTs) Use Case revolves
around minting, sending, receiving NFTs into a Cardano wallet.

NFTs are a digital asset that represents real-world objects. They can be bought
and sold online, and act as a proof of ownership for the underlying asset they
are meant to represent. Fungibility is the property of an asset to be
interchangeable with its equal value in another fungible asset (example: $1 and
10x $0.10 are interchangeable). Given that real-world objects cannot be replaced
as easily with equivalent objects is a propert reflected in the nature of NFTs.

For more details on NFT's refer to:

- [Forbes: What You Need To Know About NFT's](https://www.forbes.com/advisor/investing/nft-non-fungible-token/)
- [Cambridge Dictionary: nonfungible](https://dictionary.cambridge.org/us/dictionary/english/nonfungible)

### NFT: Progress & Planning

- Goals and status:
  - Development *TODO: add some achieved/ future goals*
    - [x] Feature Completeness as per Specifications
    - [ ] Improve Deployment Story
    - [ ] Improve Performance
    - [ ] Improve Ergonomics of Use and Installation

  - Testing
    - [x] 100% Test Coverage
    - [ ] QuickCheck Testing

  - Documentation
    - [x] Example
    - [ ] APIs

### NFT: Examples

- [NFT Demo](https://github.com/mlabs-haskell/plutus-use-cases/blob/main/mlabs/nft-demo/Main.hs)

### NFT: APIs & Endpoints

- User Endpoints:
  - Buy
    - [x] in use.
    - Description: *User buys NFT.*
  - SetPrice
    - [x] in use.
    - Description: *User sets new price for NFT.*
  
- Author Endpoints:
  - StartParams
    - [x] in use.
    - Description: *Sets the parameters to initialise a new NFT.*

- User Schemas:
  - UserSchema
    - [x] in use.
    - Description: *User schema. Owner can set the price and the buyer can try to buy.*
  - AuthorSchema
    - [x] in use.
    - Description: *Schema for the author of NFT*.

### NFT: Tests

- To run the tests:

```bash
stack test all  
```

- To see test cases refer to: `./test/Test/Nft`

### NFT: Notes

*TODO: Add any relevant notes*


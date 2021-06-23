# MLabs: Plutus Use Cases
--------------------------------------------------------------------------------
## Contents

- [Contents](#contents)
- [Overview](#overview)
  * [Prerequisites](#prerequisites)
  * [Building, Testing, Use](#building-testing-use)
  * [Documentation](#documentation)
  * [Testing](#testing)
- [Use Case: Lendex](#use-case-lendex)
  * [Description](#description)
  * [Progress & Planning](#progress--planning)
  * [Examples](#examples)
  * [APIs & Endpoints](#apis--endpoints)
  * [Notes](#notes)
- [Use Case: NFT](#use-case-nft)
  * [Description](#description-1)
  * [Progress & Planning](#progress--planning-1)
  * [Examples](#examples-1)
  * [APIs & Endpoints](#apis--endpoints-1)
  * [Notes](#notes-1)

*note: the table of contents is generated using `make readme_contents`. please
update as headings are expanded.*

## Overview

MLabs has been working on developing two Plutus Use cases, specifically:

-  [Use Case: Lendex](#use-case-lendex)

-  [Use Case: NFT](#use-case-nft)

### Prerequisites

- Git 
- Curl
- Nix

### Building, Testing, Use

*It is recommended that all current updates to your system be done before installation*

1) ***Install basic dependencies*** 

    `sudo apt install curl`
    
    `sudo apt install git`

2) ***Clone Directory***

	Create a directory and clone the project:
 
	`git clone https://github.com/mlabs-haskell/plutus-use-cases.git`
 
3) ***Install Nix***
  

   1) **Setup Nix**
	   
    $ `curl -L https://nixos.org/nix/install | sh`

      - *There is a issue with nix correctly adjusting the PATH in some machines. Please re-start your terminal and make sure Nix is in the path (`nix --version`).  Please see this discussion if you are having this issue:  https://github.com/NixOS/nix/issues/3317*

      - *This is the direct link to the nix download page for reference: https://nixos.org/download.html*

   2) **Set up binary cache** 
	
      ***Make sure to set up the IOHK binary cache. If you do not do this, you will end up building GHC, which takes several hours. If you find yourself building GHC, STOP and fix the cache.***
	
      To set up the binary cache:
		
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

Please see the original documentation at IOHK for reference: -   [How to set up the IOHK binary caches](https://github.com/input-output-hk/plutus/blob/master/README.adoc#iohk-binary-cache)

4) ***Create nix shell***
Go to the `plutus-use-cases/mlabs` directory
run the `nix-shell` command:

	$ `nix-shell` 
	- *(This will take a little while the first time)*

### Documentation

Currently the documentation is done via this document which can 
be found in the [MLabs gitHub repository](https://github.com/mlabs-haskell/plutus-use-cases/tree/main/mlabs)

### Testing
For an overview of the tests refer to the [test folder](https://github.com/mlabs-haskell/plutus-use-cases/tree/main/mlabs/test)

--------------------------------------------------------------------------------
## Use Case: Lendex 

### Description
*Small description/summary*

### Progress & Planning
- Goals and status: *add tasks and goals + their status*
  - Development
    - [x] *task 1(Done)*
    - [ ] *task 2(WIP)*

  - Testing 
    - [ ] 50% Test Coverage
    - [ ] 100% Test Coverage
    - [ ] QuickCheck Testing

  - Documentation
    - [ ] Document Examples

### Examples
- [Lendex Demo](https://github.com/mlabs-haskell/plutus-use-cases/blob/main/mlabs/lendex-demo/Main.hs)
- *Add any other relevant examples*

### APIs & Endpoints

- **API/Endpoint Name1**
  - Description: *Add Description*
  - Develop/use: *Specify if using or developing the API/Endpoint*

- **API/Endpoint Name2**
  - Description: *Add Description*
  - Develop/use: *Specify if using or developing the API/Endpoint*

### Notes
*Add any relevant notes*

--------------------------------------------------------------------------------
## Use Case: NFT

### Description
*Small description/summary*

### Progress & Planning
- Goals and status: *add tasks and goals + their status*
  - Development
    - [x] *task 1(Done)*
    - [ ] *task 2(WIP)*

  - Testing 
    - [ ] 50% Test Coverage
    - [ ] 100% Test Coverage
    - [ ] QuickCheck Testing

  - Documentation
    - [ ] Document Examples

### Examples
- [NFT Demo](https://github.com/mlabs-haskell/plutus-use-cases/blob/main/mlabs/nft-demo/Main.hs)

### APIs & Endpoints
- **API/Endpoint Name1**
  - Description: *Add Description*
  - Develop/use: *Specify if using or developing the API/Endpoint*

- **API/Endpoint Name2**
  - Description: *Add Description*
  - Develop/use: *Specify if using or developing the API/Endpoint*

### Notes
*Add any relevant notes*

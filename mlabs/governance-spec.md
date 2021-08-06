# Governance Example Spec

This will provide some simple governance functions as example behavior to be used across projects and as a simple demonstration of best-practices

After the initial scaffold, we may adjust the governance contract to perform initiation and update features for contract configuration, perhaps of some custom data, or perhaps using the existing `stablecoin` example from the plutus monorepo.

We may also move some of the implementations into an open-source library, where possible

This Contract will deal with a custom, pre minted token called GOV.  

in reality this GOV token can be any token at all, the primary purpose of the contract is to allow users to store and retreive GOV tokens, report on GOV token holdings (as they may be used in a vote), and provide rewards.

For this contract, the primitive Plutus API, and not the state machine API will be used.

## Governance Contract:

### Deposit
prerequisites: 
user has specified amount of GOV tokens

input: { amount :: Integer }

behaviors:

user must provide `amount` of GOV tokens to contract or else the contract errors out.
`amount` of GOV tokens associated with this user deposit must be reportable and we must be able to query this information knowing only the user's address PubKey

we should mint xGOV tokens (this may be a configurable behavior in a library function to match the input GOV tokens,  xGOV tokens must be returned in order to claim GOV tokens from `Withdraw`)

user cannot provide negative inputs

### Withdraw

prerequisites: 
user has successfully called deposit, 
user has specified amount of xGOV tokens in their wallet

input: { amount :: Integer }

behavior:
transfer `amount` of user-provided xGOV tokens to contract and burn them
transfer `amount` of GOV tokens to the user

if user does not have provided amount of xGOV,  error.

(because of how xGOV tokens and voting work, you must withdraw and redeposit your GOV tokens for your vote weight to change)

user cannot provide negative inputs

### ProvideRewards
Prerequisites:
user must have all the tokens and amounts in the specified Value

input: PlutusTx.Value

behaviors:
move all rewards from user wallet, divided as evenly as possible to all stakers of GOV tokens, send these tokens directly to the stakers
error out if there are no GOV tokens staked
return any remainder tokens to the caller (only for smallest possible units)

(we may create an alternative version where stakers can claim their rewards within the contract, as this is more conducive to futures markets (similar to xGOV tokens).

### QueryBalance

input: { address :: PubKey }

returns { amount :: Integer }

returns the total number of GOV tokens currently stored under the specified address. (may include multiple deposits, partial or full withdrawals may have occured)

this is used for determining vote weight in democratic procedures



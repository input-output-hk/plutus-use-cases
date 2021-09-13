module Data.UserInstance where

import Business.MarketplaceUser (UserContractId)
import Plutus.V1.Ledger.Crypto (PubKeyHash)

type UserInstance
  = { userContract :: UserContractId, userPubKey :: PubKeyHash }

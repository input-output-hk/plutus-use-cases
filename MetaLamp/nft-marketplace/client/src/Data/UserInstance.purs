module Data.UserInstance where

import Business.MarketplaceUser (UserContractId)
import Plutus.V1.Ledger.Crypto (PubKeyHash)

type UserInstance = { userInstance :: UserContractId, userPubKey :: PubKeyHash }

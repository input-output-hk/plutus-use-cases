module Mlabs.NFT.Contract.Init (
  initApp,
  getAppSymbol,
  createListHead,
) where

import PlutusTx.Prelude hiding (mconcat, (<>))
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Control.Monad (void)
import Data.Text (Text, pack)
import Text.Printf (printf)

import Plutus.Contract (Contract, mapError, ownPubKeyHash)
import Plutus.Contract qualified as Contract

import Plutus.Contracts.Currency (CurrencyError, mintContract)
import Plutus.Contracts.Currency qualified as MC
import Plutus.V1.Ledger.Value (TokenName (..), assetClass)

import Ledger (
  AssetClass,
  Value,
  scriptCurrencySymbol,
 )

import Ledger.Constraints qualified as Constraints
import Ledger.Value as Value (singleton)

import Mlabs.NFT.Types (
  GenericContract,
  MintAct (..),
  NftAppInstance (..),
  NftAppSymbol (..),
  NftListHead (..),
  UserId (..),
 )

import Data.Monoid (Last (..))

import Mlabs.NFT.Validation

{- | The App Symbol is written to the Writter instance of the Contract to be
 recovered for future opperations, and ease of use in Trace.
-}
type InitContract a = forall s. Contract (Last NftAppSymbol) s Text a

--------------------------------------------------------------------------------
-- Init --

initApp :: [UserId] -> InitContract ()
initApp admins = do
  appInstance <- createListHead admins
  let appSymbol = getAppSymbol appInstance
  Contract.tell . Last . Just $ appSymbol
  Contract.logInfo @Hask.String $ printf "Finished Initialisation: App symbol: %s" (Hask.show appSymbol)

{- | Initialise the application at the address of the script by creating the
 HEAD of the list, and coupling the one time token with the Head of the list.
-}
createListHead :: [UserId] -> GenericContract NftAppInstance
createListHead admins = do
  (uniqueToken, uniqueTokenValue) <- generateUniqueToken
  let appInstance = NftAppInstance txScrAddress uniqueToken admins
  headDatum <- nftHeadInit appInstance
  mintListHead appInstance uniqueTokenValue headDatum
  return appInstance
  where
    -- Mint the Linked List Head and its associated token.
    mintListHead :: NftAppInstance -> Value -> DatumNft -> GenericContract ()
    mintListHead appInstance uniqueTokenValue headDatum = do
      let headPolicy = mintPolicy appInstance
          emptyTokenName = TokenName PlutusTx.Prelude.emptyByteString
          proofTokenValue = Value.singleton (scriptCurrencySymbol headPolicy) emptyTokenName 1
          initRedeemer = asRedeemer Initialise
          (lookups, tx) =
            ( mconcat
                [ Constraints.typedValidatorLookups txPolicy
                , Constraints.mintingPolicy headPolicy
                ]
            , mconcat
                [ Constraints.mustPayToTheScript headDatum (proofTokenValue <> uniqueTokenValue)
                , Constraints.mustMintValueWithRedeemer initRedeemer proofTokenValue
                ]
            )
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.logInfo @Hask.String $ printf "forged HEAD for %s" (Hask.show appInstance)

    -- Contract that mints a unique token to be used in the minting of the head
    generateUniqueToken :: GenericContract (AssetClass, Value)
    generateUniqueToken = do
      self <- ownPubKeyHash
      let nftTokenName = TokenName "Unique App Token" --PlutusTx.Prelude.emptyByteString
      x <-
        mapError
          (pack . Hask.show @CurrencyError)
          (mintContract self [(nftTokenName, 1)])
      return (assetClass (MC.currencySymbol x) nftTokenName, MC.mintedValue x)

    nftHeadInit :: NftAppInstance -> GenericContract DatumNft
    nftHeadInit appInst = do
      pure
        . HeadDatum
        $ NftListHead
          { head'next = Nothing
          , head'appInstance = appInst
          }

-- | Given an App Instance return the NftAppSymbol for that app instance.
getAppSymbol :: NftAppInstance -> NftAppSymbol
getAppSymbol = NftAppSymbol . curSymbol

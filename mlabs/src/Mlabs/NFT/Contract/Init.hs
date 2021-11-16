module Mlabs.NFT.Contract.Init (
  initApp,
  getAppSymbol,
  createListHead,
) where

import Control.Monad (void)
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Text.Printf (printf)
import Prelude (mconcat, (<>))
import Prelude qualified as Hask

import Ledger (AssetClass, scriptCurrencySymbol)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value as Value (singleton)
import Plutus.Contract (Contract, mapError, ownPubKeyHash)
import Plutus.Contract qualified as Contract
import Plutus.Contracts.Currency (CurrencyError, mintContract)
import Plutus.Contracts.Currency qualified as MC
import Plutus.V1.Ledger.Value (TokenName (..), assetClass, assetClassValue)
import PlutusTx.Prelude hiding (mconcat, (<>))

import Mlabs.Data.LinkedList (LList (..))
import Mlabs.NFT.Contract.Aux (toDatum)
import Mlabs.NFT.Governance.Types (GovAct (..), GovDatum (..), GovLHead (..))
import Mlabs.NFT.Governance.Validation (govMintPolicy, govScrAddress, govScript)
import Mlabs.NFT.Types (GenericContract, MintAct (..), NftAppInstance (..), NftAppSymbol (..), NftListHead (..), UserId (..))
import Mlabs.NFT.Validation (DatumNft (..), NftTrade, asRedeemer, curSymbol, mintPolicy, txPolicy, txScrAddress)

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
  uniqueToken <- generateUniqueToken
  mintListHead $ NftAppInstance txScrAddress uniqueToken govScrAddress admins
  where
    -- Mint the Linked List Head and its associated token.
    mintListHead :: NftAppInstance -> GenericContract NftAppInstance
    mintListHead appInstance = do
      let -- Unique Token
          uniqueToken = appInstance'AppAssetClass appInstance
          uniqueTokenValue = assetClassValue uniqueToken 1
          emptyTokenName = TokenName PlutusTx.Prelude.emptyByteString
      let -- Script Head Specific Information
          headDatum :: DatumNft = nftHeadInit appInstance
          headPolicy = mintPolicy appInstance
          proofTokenValue = Value.singleton (scriptCurrencySymbol headPolicy) emptyTokenName 1
          initRedeemer = asRedeemer Initialise
      let -- Gov App Head Specific information
          govHeadDatum :: GovDatum = govHeadInit
          govHeadPolicy = govMintPolicy appInstance
          govProofTokenValue = Value.singleton (scriptCurrencySymbol govHeadPolicy) emptyTokenName 1
          govInitRedeemer = asRedeemer InitialiseGov

          -- NFT App Head
          (lookups, tx) =
            ( mconcat
                [ Constraints.typedValidatorLookups txPolicy
                , Constraints.mintingPolicy headPolicy
                , Constraints.mintingPolicy govHeadPolicy
                ]
            , mconcat
                [ Constraints.mustPayToTheScript headDatum (proofTokenValue <> uniqueTokenValue)
                , Constraints.mustPayToOtherScript (validatorHash govScript) (toDatum govHeadDatum) (govProofTokenValue <> uniqueTokenValue)
                , Constraints.mustMintValueWithRedeemer initRedeemer proofTokenValue
                , Constraints.mustMintValueWithRedeemer govInitRedeemer govProofTokenValue
                ]
            )
      void $ Contract.submitTxConstraintsWith @NftTrade lookups tx
      Contract.logInfo @Hask.String $ printf "Forged Script Head & Governance Head for %s" (Hask.show appInstance)
      return appInstance

    -- Contract that mints a unique token to be used in the minting of the head
    generateUniqueToken :: GenericContract AssetClass
    generateUniqueToken = do
      self <- ownPubKeyHash
      let nftTokenName = TokenName "Unique App Token" --PlutusTx.Prelude.emptyByteString
      x <-
        mapError
          (pack . Hask.show @CurrencyError)
          (mintContract self [(nftTokenName, 2)])
      return $ assetClass (MC.currencySymbol x) nftTokenName

    nftHeadInit :: NftAppInstance -> DatumNft
    nftHeadInit appInst =
      HeadDatum $
        NftListHead
          { head'next = Nothing
          , head'appInstance = appInst
          }

    govHeadInit =
      GovDatum $
        HeadLList
          { _head'info = GovLHead
          , _head'next = Nothing
          }

-- | Given an App Instance return the NftAppSymbol for that app instance.
getAppSymbol :: NftAppInstance -> NftAppSymbol
getAppSymbol = NftAppSymbol . curSymbol

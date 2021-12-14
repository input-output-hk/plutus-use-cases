module Mlabs.NFT.Contract.Gov.Fees (
  getFeesConstraints,
) where

import Prelude qualified as Hask

import Data.Map qualified as Map
import Data.Monoid ((<>))
import Data.Text (Text)

import Plutus.ChainIndex.Tx (txOutRefMapForAddr)
import Plutus.Contract (Contract)
import Plutus.Contract qualified as Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.V1.Ledger.Ada qualified as Ada (
  lovelaceValueOf,
 )
import PlutusTx.Prelude hiding (mconcat, mempty, (<>))

import Ledger (
  Address,
  getPubKeyHash,
  scriptCurrencySymbol,
  txOutValue,
 )
import Ledger.Typed.Scripts (Any, validatorHash, validatorScript)
import Ledger.Value as Value (TokenName (..), singleton)

import Mlabs.Data.LinkedList
import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Contract.Gov.Aux
import Mlabs.NFT.Contract.Gov.Query
import Mlabs.NFT.Governance.Types
import Mlabs.NFT.Governance.Validation (govMintPolicy, govScript)
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

-- | Returns constraints for minting GOV tokens, and paying transaction fee for given NFT
getFeesConstraints ::
  forall s.
  UniqueToken ->
  NftId ->
  Integer ->
  UserId ->
  Contract UserWriter s Text ([Constraints.TxConstraints BuiltinData BuiltinData], [Constraints.ScriptLookups Any])
getFeesConstraints uT nftId price user = do
  let ownPkh = getUserId user
  nftPi <- findNft nftId uT
  node <- case pi'data nftPi of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "getFeesConstraints:NFT not found"
  let newGovDatum = GovDatum $ NodeLList user GovLNode Nothing
      govAddr = appInstance'Governance . node'appInstance $ node
      govValidator = govScript . appInstance'UniqueToken . node'appInstance $ node
      govScriptHash = validatorHash govValidator

  feeRate <- queryCurrFeeRate uT
  feePkh <- queryFeePkh uT
  govHead' <- getGovHead govAddr
  govHead <- case govHead' of
    Just x -> Hask.pure x
    Nothing -> Contract.throwError "getFeesConstraints: GOV HEAD not found"

  govPi' <- findGovInsertPoint govAddr newGovDatum
  Contract.logInfo @Hask.String $ Hask.show $ "Gov found: " <> Hask.show govPi'
  let feeValue = round $ fromInteger price * feeRate
      mkGov name =
        Value.singleton
          (scriptCurrencySymbol govPolicy)
          (TokenName . (name <>) . getPubKeyHash $ ownPkh)
          feeValue
      mintedFreeGov = mkGov "freeGov"
      mintedListGov = mkGov "listGov"
      appInstance = node'appInstance node
      govPolicy = govMintPolicy appInstance
      govRedeemer = asRedeemer MintGov
      headPrevValue = piValue govAddr govHead
      spendHead =
        Hask.mconcat
          [ Constraints.mustSpendScriptOutput (pi'TOR govHead) govRedeemer
          , Constraints.mustPayToOtherScript
              govScriptHash
              (toDatum $ pi'data govHead)
              headPrevValue
          ]
      sharedGovTx =
        [ Constraints.mustMintValueWithRedeemer govRedeemer (mintedFreeGov <> mintedListGov)
        , Constraints.mustPayToPubKey ownPkh mintedFreeGov
        , Constraints.mustPayToPubKey feePkh (Ada.lovelaceValueOf feeValue)
        ]
      sharedGovLookup =
        [ Constraints.mintingPolicy govPolicy
        , Constraints.otherScript (validatorScript govValidator)
        , Constraints.unspentOutputs $ Map.singleton (pi'TOR govHead) (pi'CITxO govHead)
        ]
      govTx = case govPi' of
        -- Updating already existing Node
        Left govPi ->
          let prevValue =
                Ledger.txOutValue
                  . fst
                  $ (txOutRefMapForAddr govAddr (pi'CITx govPi) Map.! pi'TOR govPi)
           in [ -- Send more GOV tokens to existing Node
                Constraints.mustSpendScriptOutput (pi'TOR govPi) govRedeemer
              , Constraints.mustPayToOtherScript
                  govScriptHash
                  (toDatum . pi'data $ govPi)
                  (mintedListGov <> prevValue)
              , spendHead
              ]
        -- Inserting new Node
        Right govIp ->
          let updatedNewNode = pointNodeToMaybe' newGovDatum (fmap pi'data . next $ govIp)
              updatedPrevNode = pointNodeTo' (pi'data . prev $ govIp) updatedNewNode
              prevValue =
                Ledger.txOutValue
                  . fst
                  $ (txOutRefMapForAddr govAddr (pi'CITx . prev $ govIp) Map.! (pi'TOR . prev $ govIp))
           in [ Constraints.mustSpendScriptOutput (pi'TOR . prev $ govIp) govRedeemer
              , Constraints.mustPayToOtherScript
                  govScriptHash
                  (toDatum updatedPrevNode)
                  prevValue
              , case gov'list updatedPrevNode of
                  HeadLList {} -> Hask.mempty
                  NodeLList {} -> spendHead
              , -- Create new Node
                Constraints.mustPayToOtherScript
                  govScriptHash
                  (toDatum updatedNewNode)
                  mintedListGov
              ]
      govLookups = case govPi' of
        Left govPi ->
          [ Constraints.unspentOutputs $ Map.singleton (pi'TOR govPi) (pi'CITxO govPi)
          ]
        Right govIp ->
          [ Constraints.unspentOutputs $ Map.singleton (pi'TOR . prev $ govIp) (pi'CITxO . prev $ govIp)
          ]
  Hask.pure (govTx <> sharedGovTx, govLookups <> sharedGovLookup)

findGovInsertPoint :: Address -> GovDatum -> GenericContract (Either (PointInfo GovDatum) (InsertPoint GovDatum))
findGovInsertPoint addr node = do
  list <- getDatumsTxsOrderedFromAddr @GovDatum addr
  Contract.logInfo @Hask.String $ Hask.show $ "GOV LIST: " <> Hask.show (Hask.fmap pi'data list)
  case list of
    [] -> Contract.throwError "This should never happen." -- Unreachable
    x : xs -> findPoint x xs
  where
    findPoint x = \case
      [] -> pure $ Right $ InsertPoint x Nothing
      (y : ys) ->
        case Hask.compare (pi'data y) node of
          LT -> findPoint y ys
          EQ -> pure $ Left y
          GT -> pure $ Right $ InsertPoint x (Just y)

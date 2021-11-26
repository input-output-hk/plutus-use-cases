module Mlabs.NFT.Contract.Fees (
  getFeesConstraints,
  getCurrFeeRate,
) where

import Prelude qualified as Hask

import Data.Map qualified as Map
import Data.Maybe (fromJust)
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
import Ledger.Typed.Scripts (validatorHash, validatorScript)
import Ledger.Value as Value (TokenName (..), singleton)

import Mlabs.Data.LinkedList
import Mlabs.NFT.Contract.Aux
import Mlabs.NFT.Governance.Types
import Mlabs.NFT.Governance.Validation (govMintPolicy, govScript)
import Mlabs.NFT.Types
import Mlabs.NFT.Validation

-- | Get fee rate from GOV HEAD
getCurrFeeRate :: forall w s. UniqueToken -> Contract w s Text Rational
getCurrFeeRate uT = do
  nftHead' <- getNftHead uT
  nftHead <- case pi'data <$> nftHead' of
    Just (HeadDatum x) -> Hask.pure x
    _ -> Contract.throwError "getCurrFeeRate: NFT HEAD not found"

  let govAddr = appInstance'Governance . head'appInstance $ nftHead
  govHead' <- getGovHead govAddr
  govHead <- case gov'list . pi'data <$> govHead' of
    Just (HeadLList x _) -> Hask.pure x
    _ -> Contract.throwError "getCurrFeeRate: GOV HEAD not found"
  Hask.pure $ govLHead'feeRate govHead

-- | Returns constraints for minting GOV tokens, and paying transaction fee for given NFT
getFeesConstraints :: forall s. UniqueToken -> NftId -> Integer -> Contract UserWriter s Text ([Constraints.TxConstraints UserAct DatumNft], [Constraints.ScriptLookups NftTrade])
getFeesConstraints uT nftId price = do
  user <- getUId
  ownPkh <- Contract.ownPubKeyHash
  nftPi <- findNft nftId uT
  node <- case pi'data nftPi of
    NodeDatum n -> Hask.pure n
    _ -> Contract.throwError "getFeesConstraints:NFT not found"
  let newGovDatum = GovDatum $ NodeLList user GovLNode Nothing
      govAddr = appInstance'Governance . node'appInstance $ node
      govValidator = govScript . appInstance'UniqueToken . node'appInstance $ node
      govScriptHash = validatorHash govValidator

  feeRate <- getCurrFeeRate uT
  govHead' <- getGovHead govAddr
  govHead <- case govHead' of
    Just x -> Hask.pure x
    Nothing -> Contract.throwError "getFeesConstraints: GOV HEAD not found"

  govPi' <- findInsertPoint govAddr newGovDatum
  Contract.logInfo @Hask.String $ Hask.show $ "Gov found: " <> Hask.show govPi'
  let feeValue = round $ fromInteger price * feeRate
      mkGov name =
        Value.singleton
          (scriptCurrencySymbol govPolicy)
          (TokenName . ((name <>) . getPubKeyHash) $ ownPkh)
          feeValue
      mintedFreeGov = mkGov "freeGov"
      mintedListGov = mkGov "listGov"
      appInstance = node'appInstance node
      govPolicy = govMintPolicy appInstance
      govRedeemer = asRedeemer MintGov
      headPrevValue =
        txOutValue
          . fst
          $ (txOutRefMapForAddr govAddr (pi'CITx govHead) Map.! pi'TOR govHead)
      payFeeToHead =
        Hask.mconcat
          [ Constraints.mustSpendScriptOutput (pi'TOR govHead) govRedeemer
          , Constraints.mustPayToOtherScript
              govScriptHash
              (toDatum $ pi'data govHead)
              (Ada.lovelaceValueOf feeValue <> headPrevValue)
          ]
      sharedGovTx =
        [ Constraints.mustMintValueWithRedeemer govRedeemer (mintedFreeGov <> mintedListGov)
        , Constraints.mustPayToPubKey ownPkh mintedFreeGov
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
                txOutValue
                  . fst
                  $ (txOutRefMapForAddr govAddr (pi'CITx govPi) Map.! pi'TOR govPi)
           in [ -- Send more GOV tokens to existing Node
                Constraints.mustSpendScriptOutput (pi'TOR govPi) govRedeemer
              , Constraints.mustPayToOtherScript
                  govScriptHash
                  (toDatum . pi'data $ govPi)
                  (mintedListGov <> prevValue)
              , payFeeToHead
              ]
        -- Inserting new Node
        Right govIp ->
          let updatedNewNode = pointNodeToMaybe' newGovDatum (fmap pi'data . next $ govIp)
              updatedPrevNode = pointNodeTo' (pi'data . prev $ govIp) updatedNewNode
              prevValue =
                txOutValue
                  . fst
                  $ (txOutRefMapForAddr govAddr (pi'CITx . prev $ govIp) Map.! (pi'TOR . prev $ govIp))
           in [ case gov'list updatedPrevNode of
                  -- When inserting new node after Head, we add fee value to Head
                  HeadLList {} ->
                    Hask.mconcat
                      [ Constraints.mustSpendScriptOutput (pi'TOR . prev $ govIp) govRedeemer
                      , Constraints.mustPayToOtherScript
                          govScriptHash
                          (toDatum updatedPrevNode)
                          (prevValue <> Ada.lovelaceValueOf feeValue)
                      ]
                  -- When inserting new node after another Node (not Head), we
                  -- need to send fee to Head separately
                  NodeLList {} ->
                    Hask.mconcat
                      [ Constraints.mustSpendScriptOutput (pi'TOR . prev $ govIp) govRedeemer
                      , Constraints.mustPayToOtherScript
                          govScriptHash
                          (toDatum updatedPrevNode)
                          prevValue
                      , payFeeToHead
                      ]
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
  where
    findInsertPoint :: Address -> GovDatum -> GenericContract (Either (PointInfo GovDatum) (InsertPoint GovDatum))
    findInsertPoint addr node = do
      list <- getDatumsTxsOrderedFromAddr @GovDatum addr
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

-- `fromJust` is safe here due to on-chain constraints.
pointNodeTo' :: GovDatum -> GovDatum -> GovDatum
pointNodeTo' a b = GovDatum . fromJust $ pointNodeTo (gov'list a) (gov'list b)

pointNodeToMaybe' :: GovDatum -> Maybe GovDatum -> GovDatum
pointNodeToMaybe' a b = GovDatum . fromJust $ pointNodeToMaybe (gov'list a) (fmap gov'list b)

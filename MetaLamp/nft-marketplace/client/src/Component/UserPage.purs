module Component.UserPage where

import Prelude
import Business.Datum as Datum
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceInfo as MarketplaceInfo
import Business.MarketplaceUser (createNft, openSale, startAnAuction) as MarketplaceUser
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logError, logInfo)
import Capability.PollContract (class PollContract)
import Component.CreateNftForm as CreateNftForm
import Component.PutOnSaleForm as PutOnSaleForm
import Component.StartAnAuctionForm as StartAnAuctionForm
import Component.Utils (PageInput, runRD)
import Data.Bifunctor (lmap)
import Data.BigInteger (fromInt)
import Data.Either (Either(..))
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Network.RemoteData (RemoteData(..))
import Plutus.Contracts.NftMarketplace.OffChain.ID (UserItemId(..))
import Plutus.Contracts.NftMarketplace.OffChain.User (CreateNftParams(..), OpenSaleParams(..), StartAnAuctionParams(..)) as MarketplaceUser
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.V1.Ledger.Value (Value)
import View.NftSingletons (renderNftSingletons)

type Slot id
  = forall query. H.Slot query Void id

_userPage :: SProxy "userPage"
_userPage = SProxy

type State
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    , userFunds :: RemoteData String Value
    , marketplaceState :: RemoteData String MarketplaceDatum
    }

_userFunds :: Lens' State (RemoteData String Value)
_userFunds = prop (SProxy :: SProxy "userFunds")

_marketplaceState :: Lens' State (RemoteData String MarketplaceDatum)
_marketplaceState = prop (SProxy :: SProxy "marketplaceState")

data Action
  = Initialize
  | Reinitialize PageInput
  | GetUserFunds
  | GetMarketplaceState
  | CreateNft CreateNftForm.SubmittedNft
  | PutOnSale Datum.NftSingleton PutOnSaleForm.PriceOutput
  | PutOnAuction Datum.NftSingleton StartAnAuctionForm.DurationOutput

type Slots
  = ( createNftForm :: CreateNftForm.Slot Unit
    , putOnSaleForm :: PutOnSaleForm.Slot Datum.NftSingleton
    , putOnAuctionForm :: StartAnAuctionForm.Slot Datum.NftSingleton
    )

component ::
  forall query output m.
  LogMessages m =>
  IPFS.IPFS m =>
  PollContract m =>
  MonadEffect m =>
  MonadAff m =>
  H.Component HH.HTML query PageInput output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , initialize = Just Initialize
              , receive = Just <<< Reinitialize
              }
    }
  where
  initialState :: PageInput -> State
  initialState i =
    { userInstance: i.userInstance
    , infoInstance: i.infoInstance
    , userFunds: NotAsked
    , marketplaceState: NotAsked
    }

  render :: State -> H.ComponentHTML Action Slots m
  render st =
    HH.div_
      [ HH.h3_ [ HH.text "Wallet NFT singletons: " ]
      , renderNftSingletons st.userFunds st.marketplaceState
          $ \nft ->
              HH.div_
                [ HH.slot (SProxy :: _ "putOnSaleForm") nft PutOnSaleForm.component unit (Just <<< PutOnSale nft)
                , HH.slot (SProxy :: _ "putOnAuctionForm") nft StartAnAuctionForm.component unit (Just <<< PutOnAuction nft)
                ]
      , HH.h3_ [ HH.text "Create NFT from file: " ]
      , HH.slot (SProxy :: _ "createNftForm") unit CreateNftForm.putNftComponent unit (Just <<< CreateNft)
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      handleAction GetUserFunds
      handleAction GetMarketplaceState
      state <- H.get
      logInfo $ show state
    Reinitialize i -> do
      H.put $ initialState i
      handleAction Initialize
    GetUserFunds -> do
      state <- H.get
      runRD _userFunds $ map (lmap show)
        $ MarketplaceInfo.fundsAt state.infoInstance state.userInstance.userPubKey
    GetMarketplaceState -> do
      state <- H.get
      runRD _marketplaceState $ map (lmap show)
        $ MarketplaceInfo.marketplaceStore state.infoInstance
    CreateNft nft -> do
      ipfsCid <-
        IPFS.pinFile nft.file
          >>= case _ of
              Left e -> do
                let
                  errMsg = "could not upload to IPFS: " <> show e
                logError errMsg
                liftEffect $ throw errMsg
              Right res -> do
                logInfo $ "uploaded file to IPFS: " <> show res
                pure res
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.createNft contractId
          $ MarketplaceUser.CreateNftParams
              { cnpIpfsCid: ipfsCid
              , cnpNftName: nft.name
              , cnpNftDescription: nft.description
              , cnpNftCategory: nft.subcategories
              , cnpRevealIssuer: nft.revealIssuer
              }
      logInfo $ "Marketplace nft created: " <> show resp
      handleAction Initialize
    PutOnSale nft p -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.openSale contractId
          $ MarketplaceUser.OpenSaleParams
              { ospItemId: UserNftId nft.ipfsCid
              , ospSalePrice: fromInt p.price
              }
      logInfo $ "Marketplace nft put on sale: " <> show resp
      handleAction Initialize
    PutOnAuction nft p -> do
      contractId <- H.gets _.userInstance.userContract
      resp <-
        MarketplaceUser.startAnAuction contractId
          $ MarketplaceUser.StartAnAuctionParams
              { saapItemId: UserNftId nft.ipfsCid
              , saapDuration: wrap $ fromInt (p.duration * 1000)
              }
      logInfo $ "Marketplace nft put on auction: " <> show resp
      handleAction Initialize

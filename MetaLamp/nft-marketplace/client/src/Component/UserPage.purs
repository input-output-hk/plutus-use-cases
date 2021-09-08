module Component.UserPage where

import Prelude
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceUser (UserContractId)
import Business.MarketplaceUser as MarketplaceUser
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logError, logInfo)
import Capability.PollContract (class PollContract)
import Chain.State (handleAction)
import Component.CreateNftForm as CreateNftForm
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Exception (throw)
import Halogen (Component, lift, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Plutus.Contracts.NftMarketplace.OffChain.User as MarketplaceUser
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Web.File.File as File

type Slot id
  = forall query. H.Slot query Void id

_userPage :: SProxy "userPage"
_userPage = SProxy

type Input
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    }

type State
  = { userInstance :: UserInstance
    , infoInstance :: InfoContractId
    }

data Action
  = Initialize
  | Reinitialize Input
  | CreateNft CreateNftForm.SubmittedNft

type Slots
  = ( createNftForm :: CreateNftForm.Slot Unit )

component ::
  forall query output m.
  LogMessages m =>
  IPFS.IPFS m =>
  PollContract m =>
  MonadEffect m =>
  MonadAff m =>
  H.Component HH.HTML query Input output m
component =
  H.mkComponent
    { initialState: identity
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
  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.div_
      [ HH.h3_ [ HH.text "Create NFT from file: " ]
      , HH.slot (SProxy :: _ "createNftForm") unit CreateNftForm.putNftComponent unit (Just <<< CreateNft)
      ]

  handleAction :: Action -> H.HalogenM State Action Slots output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      logInfo $ show state
    Reinitialize st -> do
      H.put st
      handleAction Initialize
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
              , cnpRevealIssuer: true
              }
      logInfo $ "Marketplace nft created: " <> show resp
      pure unit

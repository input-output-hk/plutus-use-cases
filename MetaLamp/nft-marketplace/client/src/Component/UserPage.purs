module Component.UserPage where

import Prelude
import Business.MarketplaceInfo (InfoContractId)
import Business.MarketplaceUser (UserContractId)
import Business.MarketplaceUser as MarketplaceUser
import Capability.IPFS as IPFS
import Capability.LogMessages (class LogMessages, logError, logInfo)
import Capability.PollContract (class PollContract)
import Chain.State (handleAction)
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.UserInstance (UserInstance)
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
  | CreateNft File.File

component ::
  forall query output m.
  LogMessages m =>
  IPFS.IPFS m =>
  PollContract m =>
  MonadEffect m =>
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
  render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.div_
      [ HH.h3_ [ HH.text "Create NFT from file: " ]
      , HH.input [ HP.type_ HP.InputFile, HE.onFileUpload onNftUpload ]
      ]

  handleAction :: forall slots. Action -> H.HalogenM State Action slots output m Unit
  handleAction = case _ of
    Initialize -> do
      state <- H.get
      logInfo $ show state
    Reinitialize st -> do
      H.put st
      handleAction Initialize
    CreateNft file -> do
      ipfsCid <-
        IPFS.pinFile file
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
              , cnpNftName: "String"
              , cnpNftDescription: "String"
              , cnpNftCategory: [ "String" ]
              , cnpRevealIssuer: true
              }
      logInfo $ "Marketplace nft created: " <> show resp
      pure unit

onNftUpload :: Array File.File → Maybe Action
onNftUpload = case _ of
  [ file ] -> Just $ CreateNft file
  _ -> Nothing

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Frontend.ChooseWallet
  ( chooseWallet
  ) where

import Prelude hiding (id, (.), filter)
import Control.Category

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (First(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Vessel
import Data.Vessel.Identity
import Data.Vessel.Vessel
import Data.Vessel.ViewMorphism
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Frontend.App
import Safe

import Common.Api
import Common.Route
import Frontend.NavBar

import Language.Javascript.JSaddle

chooseWallet
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
chooseWallet = do
  navBar' Nothing
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container-fluid py-5" $ do
      elClass "h2" "display-5 fw-bold" $ text "Welcome to POKE-DEX!"
      el "p" $ text "POKE-DEX is a prototype example of how a token exchange decentralized application would behave using smart contracts on the Cardano Blockchain. Below are some crypto wallets you can choose from to play around with this DApp's features. You will be able to swap ADA for supported tokens, swap tokens, stake ADA or other tokens for liquidity, and observe the wallet's portfoilio. Don't worry, this is not spending anyone's actual ADA. Select a wallet and give it a try!"
      elClass "h3" "display-5 fw-bold" $ text "Wallet Accounts"
      elClass "p" "lead" $ text "Choose one of the available wallets below: "
      dmmWalletIds <- viewContracts
      dyn_ $ ffor dmmWalletIds $ \case
        Nothing -> do
          el "p" $ text "Loading..."
        Just [] -> do
          el "p" $ text "There are no wallets yet available."
          el "p" $ text "If the contract is still initializing, just wait. They will appear here once created."
        Just walletIds -> do
            elClass "ul" "list-group" $ do
              forM_ walletIds $ \wid -> do
                (e,_) <- elAttr' "li" ("class" =: "list-group-item list-group-item-dark" <> "style" =: "cursor:pointer") $ text wid
                setRoute $ (FrontendRoute_WalletRoute :/ (wid, WalletRoute_Swap :/ ())) <$ domEvent Click e
      elClass "h3" "display-5 fw-bold" $ text "Real Node Static Smart Contract Transaction"
      el "p" $ text "Use the button below to perform static swap using Nami Wallet against a real Alonzo Node with a smart contract that is deployed to testnet!"
      -- pabDEMV :: Dynamic t (Event t (Maybe Aeson.Value)) <- prerender (return never) $
      --   _webSocket_recv <$> jsonWebSocket ("ws://localhost:9080/ws/3af11025-ff3a-4001-866c-739a5851742c") (def :: WebSocketConfig t Aeson.Value)
      -- let pabEMV :: Event t (Maybe Aeson.Value) = switch $ current pabDEMV
      staticSwapEv <- button "Swap ADA for PikaCoin using Nami Wallet"
      ----------------------
      (addressEv, addressTrigger) <- newTriggerEvent
      dynAddress <- holdDyn "" addressEv
      staticSwapRequestEv <- prerender (pure never) $ liftJSM $ do
        let getAddressCallback :: JSCallAsFunction
            getAddressCallback = fun $ \_ _ args -> case args of
              (i:_) -> do
                fromJSVal i >>= \case
                  Just (a :: Text) -> liftIO (addressTrigger a)
                  _ -> pure ()
              _ -> pure ()
        jsWalletAddress <- eval
          ("(async function foo (someparam) { let x = await window.cardano.getUsedAddresses(); \
            \ y = CardanoWasm.BaseAddress.from_address(CardanoWasm.Address.from_bytes(buffer.Buffer.from(x[0], 'hex'))); \
            \ console.log(y.to_address().to_bech32()); \
            \ someparam(y.to_address().to_bech32()); })" :: Text)
        _ <- call jsWalletAddress jsWalletAddress (getAddressCallback)
        walletAddress <- valToText jsWalletAddress -- Note: can throw an exception
        let requestLoad = (\addr -> Api_BuildStaticSwapTransaction addr)
               <$> dynAddress
        return $ tagPromptlyDyn requestLoad staticSwapEv
      let newEv = switchDyn staticSwapRequestEv
      txBuildResponse <- requestingIdentity newEv

      widgetHold_ blank $ ffor txBuildResponse  $ \case
        Left err -> text $ T.pack err
        Right sth -> text sth

      dynEitherCborTx <- holdDyn (Left "CborTx not received") txBuildResponse
      let cborHexEv :: Event t Text = updated $ fmap (\a -> either (\err-> T.pack $ show err) (\ch -> ch) a) dynEitherCborTx
      -- TODO: Use the CBOR encoded transaction hash received to start Nami Wallet's Tx signing and submission
      (signedTxEv, signTrigger) <- newTriggerEvent
      (submitTxEv, submitTxTrigger) <- newTriggerEvent
      dynCborHex :: Dynamic t Text <- holdDyn "" cborHexEv
      dynHexWitness :: Dynamic t Text <- holdDyn "" signedTxEv

      let getSignTxCallback :: JSCallAsFunction
          getSignTxCallback = fun $ \_ _ args -> case args of
            (i:_) -> do
              fromJSVal i >>= \case
                Just (a :: Text) -> liftIO (signTrigger a)
                _ -> pure ()
            _ -> pure ()

          getSubmitTxResultCallback :: JSCallAsFunction
          getSubmitTxResultCallback = fun $ \_ _ args -> case args of
            (i:_) -> do
              fromJSVal i >>= \case
                Just (a :: Text) -> liftIO (submitTxTrigger a)
                _ -> pure ()
            _ -> pure ()

      -- when the backend responds with a build transaction, have Nami Wallet sign the transaction
      prerender_ blank $ performEvent_ $ (\cborHexTx -> liftJSM $ do
        signedTx <-
          eval ("(async function foo (someparam) { let x = await window.cardano.signTx('" <> cborHexTx <> "', true); console.log(x); someparam(x); })" :: T.Text)
        _ <- call signedTx signedTx (getSignTxCallback)
        return ()
        ) <$> cborHexEv

      let dynTxSubmissionInput :: Dynamic t (Text, Text) = ffor2 dynCborHex dynHexWitness $ \txHex txWit -> (txHex, txWit)
          txSubmissionEv = tagPromptlyDyn dynTxSubmissionInput signedTxEv

      -- append new txWitness after signing and submit tx to chain
      prerender_ blank $ performEvent_ $ (\(initialTxHash, txWitness) -> liftJSM $ do
        submitTxResult <-
          eval ("(async function foo (someparam) { let x = await appendAndSubmit('" <> initialTxHash <> "','" <> txWitness <> "'); console.log(x); someparam(x); })" :: T.Text)
        _ <- call submitTxResult submitTxResult (getSubmitTxResultCallback)
        return ()
        ) <$> txSubmissionEv


      display dynHexWitness
      widgetHold_ blank $ ffor submitTxEv $ \msg -> el "p" $ text msg
      return ()

viewContracts
  :: ( MonadQuery t (Vessel Q (Const SelectedCount)) m
     , Reflex t
     )
  => m (Dynamic t (Maybe [Text]))
viewContracts = (fmap.fmap.fmap) (Map.elems . Map.mapMaybe getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_ContractList . identityV

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

import Prelude hiding (id, filter)

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Common.Route
import Frontend.NavBar

import Language.Javascript.JSaddle

chooseWallet
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
chooseWallet = do
  navBar' Nothing
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container py-5" $ divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h2" "display-5 fw-bold" $ text "Welcome to POKE-DEX!"
      elClass "h3" "display-5 fw-bold" $ text "Real Node Static Smart Contract Transaction"
      el "p" $ text "Use the button below to perform static swap using Nami Wallet against a real Alonzo Node with a smart contract that is deployed to testnet!"
      inputAmount <- inputElement $ def & initialAttributes .~ ("type" =: "number" <> "placeholder" =: "Enter amount of ADA")
      let dynAdaAmount =  ((either (\_ -> 0) fst) . T.decimal) <$> _inputElement_value inputAmount
      staticSwapEv <- button "Swap ADA to receive PikaCoin"
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
        let requestLoad = (\addr adaAmount -> Api_BuildStaticSwapTransaction addr adaAmount)
               <$> dynAddress
               <*> dynAdaAmount
        return $ tagPromptlyDyn requestLoad staticSwapEv
      let newEv = switchDyn staticSwapRequestEv
      txBuildResponse <- requestingIdentity newEv
      -- Use the CBOR encoded transaction hash received to start Nami Wallet's Tx signing and submission
      let (txBuildFailEv, cborHexEv) = fanEither txBuildResponse

      widgetHold_ blank $ ffor txBuildFailEv  $ \errMsg -> el "p" $ text $ T.pack errMsg
      widgetHold_ blank $ ffor cborHexEv  $ \_ -> el "p" $ text $ T.pack "Transaction built"
      (signedTxEv, signTrigger) <- newTriggerEvent
      (submitTxEv, submitTxTrigger) <- newTriggerEvent
      dynCborHex :: Dynamic t (Text, Text) <- holdDyn ("", "") cborHexEv
      dynHexWitness :: Dynamic t Text <- holdDyn "" signedTxEv

      -- helper functions to return js callback results as Text
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
      prerender_ blank $ performEvent_ $ (\(cborHexTx,_) -> liftJSM $ do
        signedTx <-
          eval ("(async function foo (someparam) { let x = await window.cardano.signTx('" <> cborHexTx <> "', true); console.log(x); someparam(x); })" :: T.Text)
        _ <- call signedTx signedTx (getSignTxCallback)
        return ()
        ) <$> cborHexEv

      let dynTxSubmissionInput :: Dynamic t (Text, Text) = ffor2 dynCborHex dynHexWitness $ \(txHex,_) txWit -> (txHex, txWit)
          txSubmissionEv = tagPromptlyDyn dynTxSubmissionInput signedTxEv

      -- append new txWitness after signing and submit tx to chain
      prerender_ blank $ performEvent_ $ (\(initialTxHash, txWitness) -> liftJSM $ do
        submitTxResult <-
          eval ("(async function foo (someparam) { let x = await appendAndSubmit('" <> initialTxHash <> "','" <> txWitness <> "'); console.log(x); someparam(x); })" :: T.Text)
        _ <- call submitTxResult submitTxResult (getSubmitTxResultCallback)
        return ()
        ) <$> txSubmissionEv

      -- confirm the swap was successful
      let successfulSubmissionEv = ffilter (\ev -> ev == "undefined") submitTxEv
          successLoad = (\(_, proposalId) -> Api_ConfirmSwapSuccess proposalId)
             <$> dynCborHex
          requestConfirmSwapSuccess = tagPromptlyDyn successLoad successfulSubmissionEv
      _ <- requestingIdentity requestConfirmSwapSuccess

      -- show tx submittion results
      widgetHold_ blank $ ffor submitTxEv $ \msg -> el "p" $ text $ case msg of
        "undefined" -> "Transaction successfully submitted"
        _ -> msg
      return ()


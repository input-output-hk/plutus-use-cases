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
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Reflex.Dom.Core
import Rhyolite.Frontend.App

import Common.Api
import Frontend.NavBar

import Language.Javascript.JSaddle

chooseWallet
  :: forall t m js
  .  ( MonadRhyoliteWidget (DexV (Const SelectedCount)) Api t m
     , Prerender js t m
     )
  => m ()
chooseWallet = mdo
  pb <- getPostBuild
  enableNamiEv <- navBar dynAddress
  (addressEv, addressTrigger) <- newTriggerEvent
  dynAddress <- holdDyn "" addressEv
  divClass "p-5 mb-4 bg-light rounded-5" $ do
    divClass "container py-5" $ divClass "pricing-header px-3 py-3 pt-md-5 pb-md-4 mx-auto text-center" $ do
      elClass "h2" "display-5 fw-bold mb-5" $ text "SampleSwap"
      elClass "p" "mb-5" $ text "One paragraph description - COPY TBD"
      divClass "card mb-5" $ divClass "card-body" $ divClass "form-container px-5" $ divClass "form-group" $ do
        inputAmount <- divClass "input-group row mt-5 mb-1 mr-3 ml-3" $ do
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-select mx-3" <> "type" =: "text" <> "placeholder" =: "tADA" <> "disabled" =: "")
          inpAmount <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "number" <> "placeholder" =: "0.0")
          return inpAmount
        _ <- divClass "input-group row mt-1 mb-1 mx-3" $ text "for"
        _ <- divClass "input-group row mt-1 mb-5 mr-3 ml-3" $ do
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-select mx-3" <> "type" =: "text" <> "placeholder" =: "PikaCoin" <> "disabled" =: "")
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "number" <> "placeholder" =: "0.0" <> "disabled" =: "")
          return ()
        let dynAdaAmount =  ((either (\_ -> 0) fst) . T.decimal) <$> _inputElement_value inputAmount
        staticSwapEv <- divClass "input-group row mt-3 mb-5" $ do
          (e,_) <- elClass' "button" "btn btn-dark" $ text "SWAP"
          return $ domEvent Click e
        ----------------------
        prerender_ blank $ performEvent_ $ (liftJSM $ do
          let getAddressCallback :: JSCallAsFunction
              getAddressCallback = fun $ \_ _ args -> case args of
                (i:_) -> do
                  fromJSVal i >>= \case
                    Just (a :: Text) -> liftIO (addressTrigger a)
                    _ -> pure ()
                _ -> pure ()
          jsWalletAddress <- eval
            ("(async function foo (someparam) { let z = await window.cardano.isEnabled(); if (z) { let x = await window.cardano.getUsedAddresses(); \
              \ y = CardanoWasm.BaseAddress.from_address(CardanoWasm.Address.from_bytes(buffer.Buffer.from(x[0], 'hex'))); \
              \ console.log(y.to_address().to_bech32()); \
              \ someparam(y.to_address().to_bech32());} })" :: Text)
          _ <- call jsWalletAddress jsWalletAddress (getAddressCallback)
          return ()) <$ (leftmost [pb, () <$ enableNamiEv])
        let requestLoad = (\addr adaAmount -> Api_BuildStaticSwapTransaction addr adaAmount)
               <$> dynAddress
               <*> dynAdaAmount
        let staticSwapRequestEv =tagPromptlyDyn requestLoad staticSwapEv
        -- let newEv = switchDyn staticSwapRequestEv
        txBuildResponse <- requestingIdentity staticSwapRequestEv
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
      elAttr "a" ("href" =: "https://github.com/obsidiansystems/plutus-use-cases/tree/is-tire-kick") $ elClass "button" "btn btn-outline-dark mt-5" $ text "Learn More"


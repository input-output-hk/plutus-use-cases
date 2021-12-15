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
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Map (Map)
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
     , MonadIO (Performable m)
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
      elClass "p" "mb-5" $ text "" -- placeholder for swap description
      divClass "card mb-5" $ divClass "card-body" $ divClass "form-container px-5" $ divClass "form-group" $ mdo
        inputAmount <- divClass "input-group row mt-5 mb-1 mr-3 ml-3" $ do
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "text" <> "placeholder" =: "tADA" <> "disabled" =: "")
          inpAmount <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "number" <> "placeholder" =: "0.0")
                                          & inputElementConfig_setValue .~ ("0" <$ submitTxEv)
          return inpAmount
        _ <- divClass "input-group row mt-1 mb-1 mx-3" $ text "for"
        _ <- divClass "input-group row mt-1 mb-5 mr-3 ml-3" $ do
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "text" <> "placeholder" =: "OBSIDIAN" <> "disabled" =: "")
          _ <- inputElement $ def & initialAttributes .~ ("class" =: "form-control mx-3" <> "type" =: "text" <> "placeholder" =: "0.0" <> "disabled" =: "")
                                  & inputElementConfig_setValue .~
                                      leftmost [ (fmap (\resp -> either (\_ -> T.pack $ "~" <> show (0 :: Integer)) (\swapAmt -> ("~" <> (T.pack $ show $ swapAmt))) resp) respEstimateSwap)
                                               , ("0" <$ submitTxEv)
                                               ]
          return ()
        let dynAdaAmount =  ((either (\_ -> 0) fst) . T.decimal) <$> _inputElement_value inputAmount
        -- Get estimated amount of arbitrary token to receive when swap has completed
            requestEstimateSwap = (\amt -> Api_EstimateSwap amt)
               <$> (updated dynAdaAmount)
        respEstimateSwap <- requestingIdentity requestEstimateSwap
        btnEnabled <- holdDyn ButtonStatus_Ready $ leftmost
                                                 [ ButtonStatus_Busy <$ staticSwapEv
                                                 , ButtonStatus_Ready <$ txBuildResponse
                                                 , ((\amt -> if amt > 0 then ButtonStatus_Ready else ButtonStatus_Disabled) <$> (updated dynAdaAmount))
                                                 ]
        staticSwapEv <- divClass "input-group row mt-3 mb-5" $ do
          (e,_) <- elDynAttr' "button" (toggleBtnUsability <$> btnEnabled) $ do
            elDynAttr "span" (toggleSpinner <$> btnEnabled) blank
            dyn_ $ ffor btnEnabled $ \case
              ButtonStatus_Ready -> text "SWAP"
              ButtonStatus_Busy -> text "Loading..."
              ButtonStatus_Disabled -> text "SWAP"
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
        let staticSwapRequestEv = tagPromptlyDyn requestLoad staticSwapEv
        -- let newEv = switchDyn staticSwapRequestEv
        txBuildResponse <- requestingIdentity staticSwapRequestEv
        -- Use the CBOR encoded transaction hash received to start Nami Wallet's Tx signing and submission
        let (txBuildFailEv, cborHexEv) = fanEither txBuildResponse

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

        -- show tx submission results
        let buildTransactionFailedEv =  ffor txBuildFailEv $ \errMsg -> UIMessage_Failure (T.pack $ show errMsg)
            buildTransactionSuccessEv =  ffor cborHexEv $ \_ -> UIMessage_Success "Transaction built"
            submitTransactionSuccessEv =  ffor submitTxEv $ \msg -> case msg of
              "undefined" -> UIMessage_Success "Transaction successfully submitted"
              _ -> UIMessage_Failure (T.pack $ show msg)
        refreshEv <- delay 5 submitTransactionSuccessEv
        let refreshSwapWidget = UIMessage_None <$ refreshEv
            uiMessageEv = leftmost [buildTransactionFailedEv, buildTransactionSuccessEv, submitTransactionSuccessEv, refreshSwapWidget]
        widgetHold_ blank $ ffor uiMessageEv $ \case
          UIMessage_Success msg -> elClass "p" "text-success" $ text msg
          UIMessage_Failure msg -> elClass "p" "text-danger" $ text msg
          UIMessage_None -> blank
      elAttr "a" ("href" =: "https://github.com/input-output-hk/plutus-use-cases/tree/obsidian-systems/dex" <> "target" =: "_blank") $ elClass "button" "btn btn-outline-dark mt-5" $ text "Learn More"

data ButtonStatus = ButtonStatus_Ready
                  | ButtonStatus_Busy
                  | ButtonStatus_Disabled
  deriving (Eq, Show)

data UIMessages = UIMessage_Success Text
                | UIMessage_Failure Text
                | UIMessage_None
  deriving (Eq, Show)

toggleBtnUsability :: ButtonStatus -> Map Text Text
toggleBtnUsability ButtonStatus_Ready = ("class" =: "btn btn-lg btn-block btn-dark")
toggleBtnUsability ButtonStatus_Busy = ("class" =: "btn btn-lg btn-block btn-light" <> "disabled" =: "true")
toggleBtnUsability ButtonStatus_Disabled = ("class" =: "btn btn-lg btn-block btn-secondary" <> "disabled" =: "true")

toggleSpinner :: ButtonStatus -> Map Text Text
toggleSpinner ButtonStatus_Ready = ("class" =: "d-none")
toggleSpinner ButtonStatus_Busy = ("class" =: "spinner-border spinner-border-sm" <> "role" =: "status" <> "aria-hidden" =: "true")
toggleSpinner ButtonStatus_Disabled = ("class" =: "d-none")


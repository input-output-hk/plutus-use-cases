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
      el "p" $ text "Use the button below to perform static swap using Nami Wallet against a real Alonzo Node with a smart contract that is deployed to test net magic 8!"
      staticSwapEv <- button "Swap ADA for PikaCoin"
      staticSwapRequestEv <- prerender (pure never) $ liftJSM $ do
        jsWalletAddress <- eval ("window.cardano.getBalance().then(result => result);" :: Text)
        walletAddress <- valToText jsWalletAddress -- Note: can throw an exception
        jsWalletUtxo <- eval ("(async () => { let x = await window.cardano.getUtxos(); console.log(x); return x;})();" :: Text)
        -- cdno <- jsg "window" ! "cardano"
        -- jsWalletUtxo2 <- cdno ^. js0 "getUtxos"
        jsonWalletUtxo <- valToJSON jsWalletUtxo
        let aesonValToText :: Aeson.Value -> Text
            aesonValToText val = case val of
              Aeson.String txtVal -> txtVal
              _ -> ("" :: Text)
            jsValueWalletUtxo = toJSON jsonWalletUtxo
            walletUtxos = headMay $ map aesonValToText $ case jsValueWalletUtxo of
              Aeson.Array utxoVector -> V.toList utxoVector
              _ -> []
        liftIO $ print $ show jsonWalletUtxo
        jsCollateralUtxo <- eval ("window.cardano.getCollateral().then(result => result);" :: Text)
        jsonCollateralUtxo <- valToJSON jsWalletUtxo
        let jsValueCollateralUtxo = toJSON jsonCollateralUtxo
            collateralUtxos = headMay $ map aesonValToText $ case jsValueCollateralUtxo of
              Aeson.Array utxoVector -> V.toList utxoVector
              _ -> []
        liftIO $ print $ show jsonCollateralUtxo
        let requestLoad = Api_BuildStaticSwapTransaction
               (fromMaybe ""  walletUtxos)
               (fromMaybe ""  collateralUtxos)
               walletAddress
        return $ requestLoad <$ staticSwapEv
        return never
      let newEv = switchDyn staticSwapRequestEv
      staticSwapResponse <- requestingIdentity newEv
      widgetHold blank $ ffor staticSwapResponse $ \case
        Left err -> text $ T.pack err
        Right sth -> text sth
      return ()

viewContracts
  :: ( MonadQuery t (Vessel Q (Const SelectedCount)) m
     , Reflex t
     )
  => m (Dynamic t (Maybe [Text]))
viewContracts = (fmap.fmap.fmap) (Map.elems . Map.mapMaybe getFirst . runIdentity) $ queryViewMorphism 1 $ constDyn $ vessel Q_ContractList . identityV

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Plutus.Abstract.ContractResponse where

import qualified Control.Lens                     as Lens
import           Control.Monad                    hiding (fmap)
import qualified Data.Aeson                       as J
import qualified Data.Map.Strict                  as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           GHC.Generics                     (Generic)
import           GHC.TypeLits                     (KnownSymbol, symbolVal)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Abstract.RemoteData       (RemoteData (..))
import           Plutus.Contract                  hiding (when)
import           Plutus.Contracts.Currency        as Currency
import           Plutus.V1.Ledger.Ada             (adaValueOf, lovelaceValueOf)
import qualified Plutus.V1.Ledger.Address         as Addr
import           Plutus.V1.Ledger.Value           as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap                as AssocMap
import           PlutusTx.Prelude                 hiding (Monoid (..),
                                                   Semigroup (..), mconcat,
                                                   unless)
import           Prelude                          (Monoid (..), Semigroup (..),
                                                   show, subtract)
import qualified Prelude

withRemoteDataResponse :: forall l a p r s.
    (HasEndpoint l p s, FromJSON p)
    => Proxy l
    -> (a -> r)
    -> (p -> Contract (RemoteData Text r) s Text a)
    -> Promise (RemoteData Text r) s Void ()
withRemoteDataResponse ep g c = do
    handleEndpoint @l $ \case
        Left err -> tell $ Failure err
        Right p -> do
            _ <- tell Loading
            e <- runError $ errorHandler `handleError` c p
            tell $ case e of
                Left err -> Failure err
                Right a  -> Success $ g a

errorHandler :: Text -> Contract w s Text b
errorHandler e = do
    logInfo @Text ("Error submiting the transaction: " <> e)
    throwError e

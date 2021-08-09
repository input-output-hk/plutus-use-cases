{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Plutus.Abstract.ContractResponse where

import qualified Control.Lens                     as Lens
import           Control.Monad                    hiding (fmap)
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as Map
import           Data.Monoid                      (Last (..))
import           Data.Proxy                       (Proxy (..))
import           Data.Text                        (Text, pack)
import qualified Data.Text                        as Text
import           Data.Void                        (Void)
import           Ledger                           hiding (singleton)
import           Ledger.Constraints               as Constraints
import           Ledger.Constraints.OnChain       as Constraints
import           Ledger.Constraints.TxConstraints as Constraints
import qualified Ledger.Scripts                   as Scripts
import qualified Ledger.Typed.Scripts             as Scripts
import           Playground.Contract
import           Plutus.Abstract.OutputValue      (OutputValue (..))
import qualified Plutus.Abstract.TxUtils          as TxUtils
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
import           Text.Printf                      (printf)

data ContractResponse e a = ContractSuccess a | ContractError e | ContractPending
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Semigroup (ContractResponse e a) where
    a <> b = b

instance Monoid (ContractResponse e a) where
    mempty = ContractPending
    mappend = (<>)

withContractResponse :: forall l a p r s.
    (HasEndpoint l p s, FromJSON p)
    => Proxy l
    -> (a -> r)
    -> (p -> Contract (ContractResponse Text r) s Text a)
    -> Contract (ContractResponse Text r) s Void ()
withContractResponse _ g c = do
    e <- runError $ do
        p <- endpoint @l
        _ <- tell ContractPending
        errorHandler `handleError` c p
    tell $ case e of
        Left err -> ContractError err
        Right a  -> ContractSuccess $ g a

errorHandler :: Text -> Contract w s Text b
errorHandler e = do
    logInfo @Text ("Error submiting the transaction: " <> e)
    throwError e

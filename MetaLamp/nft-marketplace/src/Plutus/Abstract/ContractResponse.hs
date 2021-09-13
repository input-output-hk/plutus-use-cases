{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

module Plutus.Abstract.ContractResponse where

import qualified Control.Lens                     as Lens
import           Control.Monad                    hiding (fmap)
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

data ContractResponse e a = CrSuccess a | CrError e | CrPending
    deriving stock    (Prelude.Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

Lens.makeClassyPrisms ''ContractResponse

instance Semigroup (ContractResponse e a) where
    a <> b = b

instance Monoid (ContractResponse e a) where
    mempty = CrPending
    mappend = (<>)

withContractResponse :: forall l a p r s.
    (HasEndpoint l p s, FromJSON p)
    => Proxy l
    -> (a -> r)
    -> (p -> Contract (ContractResponse Text r) s Text a)
    -> Promise (ContractResponse Text r) s Void ()
withContractResponse _ g c = do
    handleEndpoint @l $ \case
        Left err -> tell $ CrError err
        Right p -> do
            _ <- tell CrPending
            e <- runError $ errorHandler `handleError` c p
            tell $ case e of
                Left err -> CrError err
                Right a  -> CrSuccess $ g a

errorHandler :: Text -> Contract w s Text b
errorHandler e = do
    logInfo @Text ("Error submiting the transaction: " <> e)
    throwError e

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
import qualified Test.QuickCheck                  as Q
import           Text.Printf                      (printf)

newtype ContractResponse e a =
  ContractResponse
    { getEndpointResponses :: Map.Map Prelude.String (RemoteData e a)
    }
  deriving  (Prelude.Eq, Prelude.Show, Generic)
  deriving anyclass (J.ToJSON, J.FromJSON)
  deriving newtype (Q.Arbitrary)

instance Semigroup (ContractResponse e a) where
  (ContractResponse x) <> (ContractResponse y) = ContractResponse $ Map.unionWith (<>) x y

instance Monoid (ContractResponse e a) where
  mempty = ContractResponse mempty

updateEndpointStatus :: forall a e proxy l. (KnownSymbol l) => proxy l -> RemoteData e a -> ContractResponse e a
updateEndpointStatus p status = ContractResponse $ Map.singleton label status
  where
    label :: Prelude.String
    label = symbolVal p

getEndpointStatus :: forall a e proxy l. (KnownSymbol l) => proxy l -> ContractResponse e a -> RemoteData e a
getEndpointStatus p (ContractResponse res) = fromMaybe NotAsked $ Map.lookup label res
  where
    label :: Prelude.String
    label = symbolVal p

withContractResponse :: forall l a p r s.
    (HasEndpoint l p s, FromJSON p)
    => Proxy l
    -> (a -> r)
    -> (p -> Contract (ContractResponse Text r) s Text a)
    -> Promise (ContractResponse Text r) s Void ()
withContractResponse ep g c = do
    handleEndpoint @l $ \case
        Left err -> tellEndpointStatus ep $ Failure err
        Right p -> do
            _ <- tellEndpointStatus ep Loading
            e <- runError $ errorHandler `handleError` c p
            tellEndpointStatus ep $ case e of
                Left err -> Failure err
                Right a  -> Success $ g a

errorHandler :: Text -> Contract w s Text b
errorHandler e = do
    logInfo @Text ("Error submiting the transaction: " <> e)
    throwError e

tellEndpointStatus :: forall a s e ce proxy l m. (KnownSymbol l) => proxy l -> RemoteData e a -> Contract (ContractResponse e a) s ce ()
tellEndpointStatus p status = tell $ updateEndpointStatus p status

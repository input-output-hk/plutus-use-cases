module View.NftSingletons where

import Prelude
import Business.Datum as Datum
import Halogen.HTML as HH
import Network.RemoteData (RemoteData)
import Plutus.Contracts.NftMarketplace.OnChain.Core.StateMachine (MarketplaceDatum)
import Plutus.V1.Ledger.Value (Value)
import View.RemoteDataState (remoteDataState)

renderNftSingletons ::
  forall props act.
  RemoteData String Value -> RemoteData String MarketplaceDatum -> HH.HTML props act
renderNftSingletons val md = remoteDataState render ({ value: _, datum: _ } <$> val <*> md)
  where
  render rd = HH.div_ $ map (HH.text <<< show) $ Datum.findNftSingletons rd.value rd.datum

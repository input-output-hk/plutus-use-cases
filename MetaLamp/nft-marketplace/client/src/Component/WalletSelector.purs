module Component.WalletSelector where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Symbol (SProxy(..))

type WalletSelectorSlot id
  = forall query. H.Slot query Output id

_walletSelector = SProxy :: SProxy "walletSelector"

data UserWallet
  = WalletA
  | WalletB
  | WalletC

derive instance eqUserWallet :: Eq UserWallet

derive instance ordUserWallet :: Ord UserWallet

derive instance genericUserWallet :: Generic UserWallet _

instance showUserWallet :: Show UserWallet where
  show = genericShow

userWallets :: Array UserWallet
userWallets = [ WalletA, WalletB, WalletC ]

fromString :: String -> Maybe UserWallet
fromString "WalletA" = Just WalletA

fromString "WalletB" = Just WalletB

fromString "WalletC" = Just WalletC

fromString _ = Nothing

data Output
  = Submit UserWallet

data Action
  = Choose UserWallet

type State
  = { activeWallet :: UserWallet }

initialState :: forall i. i -> State
initialState _ = { activeWallet: WalletA }

component :: forall query m i. H.Component HH.HTML query i Output m
component =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  render :: forall w. State -> HH.HTML w Action
  render state =
    HH.div_
      [ HH.select
          [ HP.value $ show state.activeWallet, HE.onValueChange (map Choose <<< fromString) ]
          ( map
              ( \w ->
                  HH.option [ HP.value $ show w, HP.selected (w == state.activeWallet) ] [ HH.text $ show w ]
              )
              userWallets
          )
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Choose wallet -> do
      H.modify_ _ { activeWallet = wallet }
      H.raise $ Submit wallet

{-# LANGUAGE GADTs #-}

module Test.NFT.QuickCheck where

import Data.String (IsString (..))
import PlutusTx.Prelude hiding (fmap, length, mconcat, unless, (<$>), (<*>), (==))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Prelude (div, fmap, (<$>), (<*>), (==))
import Prelude qualified as Hask

import Mlabs.NFT.Api
import Mlabs.NFT.Contract
import Mlabs.NFT.Types
import Test.NFT.Init

-- data NftModel = NftModel
--   { _mPrice :: Maybe Integer
--   , _mOwner :: Wallet
--   , _mAuthor :: Wallet
--   , _mMinted :: Bool
--   , _mWallets :: Map Wallet Integer
--   }
--   deriving (Hask.Show, Hask.Eq)
-- makeLenses ''NftModel

-- instance ContractModel NftModel where
--   data Action NftModel
--     = ActionMint
--     | ActionSetPrice Wallet (Maybe Integer)
--     | ActionBuy Wallet Integer (Maybe Integer)
--     deriving (Hask.Show, Hask.Eq)

--   data ContractInstanceKey NftModel w s e where
--     Key :: Wallet -> ContractInstanceKey NftModel (Last NftId) NFTAppSchema Text

--   instanceTag key _ = fromString $ Hask.show key

--   arbitraryAction _ =
--     QC.oneof
--       [ Hask.pure ActionMint
--       , ActionSetPrice <$> genWallet <*> QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
--       , ActionBuy <$> genWallet <*> genNonNeg <*> QC.oneof [Hask.pure Nothing, Just <$> genNonNeg]
--       ]

--   initialState = NftModel Nothing w1 w1 False $ Map.fromList [(w1, 1000), (w2, 1000), (w3, 1000)]

--   nextState ActionMint = do
--     mMinted $= True
--   nextState (ActionSetPrice wal price) = do
--     s <- view contractState <$> getModelState
--     if s ^. mOwner == wal && s ^. mMinted
--       then mPrice $= price
--       else Hask.pure ()
--     wait 10
--   nextState (ActionBuy wal price newPrice) = do
--     s <- view contractState <$> getModelState
--     let currPrice = s ^. mPrice
--     let authorShare = price `div` 10
--     let ownerShare = price - authorShare
--     if s ^. mWallets . at wal >= Just price && Just price >= currPrice && isJust currPrice && s ^. mMinted
--       then do
--         (mWallets . at (s ^. mAuthor)) $~ fmap (+ authorShare)
--         (mWallets . at (s ^. mOwner)) $~ fmap (+ ownerShare)
--         (mWallets . at wal) $~ fmap (Hask.subtract price)
--         mOwner $= wal
--         mPrice $= newPrice
--       else Hask.pure ()
--     wait 10

--   precondition s ActionMint = not (s ^. contractState . mMinted)
--   precondition s _ = s ^. contractState . mMinted

--   perform _ s ActionMint = do
--     unless (s ^. contractState . mMinted) $ do
--       hdl <- activateContractWallet w1 endpoints
--       void $ callEndpoint @"mint" hdl mp
--       void $ waitNSlots 10
--       Last _ <- observableState hdl
--       Hask.pure ()
--   perform _ s (ActionBuy wal price newPrice) = do
--     when (s ^. contractState . mMinted) $ do
--       hdl <- activateContractWallet wal endpoints
--       Last nid <- observableState hdl
--       case nid of
--         Just nftId -> callEndpoint @"buy" hdl (BuyRequestUser nftId price newPrice)
--         Nothing -> throwError $ GenericError "NFT not minted"
--       void $ waitNSlots 10
--   perform _ s (ActionSetPrice wal price) = do
--     when (s ^. contractState . mMinted) $ do
--       hdl <- activateContractWallet wal endpoints
--       Last nid <- observableState hdl
--       case nid of
--         Just nftId -> callEndpoint @"set-price" hdl (SetPriceParams nftId price)
--         Nothing -> throwError $ GenericError "NFT not minted"
--       void $ waitNSlots 10

-- deriving instance Hask.Eq (ContractInstanceKey NftModel w s e)
-- deriving instance Hask.Show (ContractInstanceKey NftModel w s e)

-- wallets :: [Wallet]
-- wallets = [w1, w2, w3]

-- -- | Random wallet
-- genWallet :: QC.Gen Wallet
-- genWallet = QC.elements wallets

-- -- | Random non negative integer
-- genNonNeg :: QC.Gen Integer
-- genNonNeg = QC.getNonNegative <$> QC.arbitrary

-- instanceSpec :: [ContractInstanceSpec NftModel]
-- instanceSpec = Hask.pure $ ContractInstanceSpec (Key w1) w1 endpoints

-- propContract :: Actions NftModel -> QC.Property
-- propContract =
--   propRunActionsWithOptions
--     checkOptions
--     instanceSpec
--     (const $ Hask.pure True)

test :: TestTree
test = testGroup "QuickCheck" [] -- [testProperty "Contract" propContract]

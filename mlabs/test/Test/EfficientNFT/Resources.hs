module Test.EfficientNFT.Resources (test) where

import Prelude hiding (fromEnum, toEnum)

import Control.Monad (void, (<=<))
import Control.Monad.State.Strict (modify)
import Data.Default (def)
import Data.List (find)
import Data.Maybe (fromJust)
import Ledger (
  Extended (Finite, PosInf),
  Interval (Interval),
  LowerBound (LowerBound),
  PaymentPubKeyHash (PaymentPubKeyHash),
  PubKeyHash,
  UpperBound (UpperBound),
  minAdaTxOut,
  scriptCurrencySymbol,
  txOutValue,
  unPaymentPubKeyHash,
 )
import Ledger.TimeSlot (slotToBeginPOSIXTime)
import Ledger.Typed.Scripts (validatorHash)
import Ledger.Value (Value, singleton, unAssetClass, valueOf)
import Mlabs.EfficientNFT.Dao (daoValidator)
import Mlabs.EfficientNFT.Lock (lockValidator)
import Mlabs.EfficientNFT.Marketplace (marketplaceValidator)
import Mlabs.EfficientNFT.Token (mkTokenName, policy)
import Mlabs.EfficientNFT.Types (
  LockAct (Unstake),
  LockDatum (LockDatum),
  MarketplaceAct (Redeem, Update),
  MintAct (BurnToken, ChangeOwner, ChangePrice, MintToken),
  NftCollection (NftCollection),
  NftData (NftData),
  NftId (NftId),
  nftCollection'author,
  nftCollection'authorShare,
  nftCollection'collectionNftCs,
  nftCollection'daoScript,
  nftCollection'daoShare,
  nftData'nftCollection,
  nftData'nftId,
  nftId'collectionNftTn,
  nftId'owner,
  nftId'price,
 )
import Plutus.Test.Model (
  BchConfig,
  FakeCoin (FakeCoin),
  Run,
  addMintRedeemer,
  bchConfig,
  bchConfigSlotConfig,
  currentSlot,
  fakeCoin,
  fakeValue,
  filterSlot,
  mintValue,
  newUser,
  payToPubKey,
  payToScript,
  payToScriptHash,
  payWithDatumToPubKey,
  scriptBoxAt,
  sendTx,
  signTx,
  spend,
  spendBox,
  testLimits,
  txBoxOut,
  userSpend,
  validateIn,
 )
import Plutus.V1.Ledger.Ada (getLovelace, lovelaceValueOf, toValue)
import Plutus.V1.Ledger.Api (toBuiltinData)
import PlutusTx.Enum (fromEnum, toEnum)
import PlutusTx.Prelude (divide)
import Test.Tasty (TestTree, testGroup)

-- test :: TestTree
test :: BchConfig -> TestTree
test cfg =
  testGroup
    "Resources usage"
    [ good "Seabug scripts" 2 seabugActions
    ]
  where
    good msg n act =
      testLimits
        initFunds
        cfg
        msg
        (filterSlot (> n))
        --  uncomment to see stats, it introduces fake error, script will fail but we can see the results
        --  $ (>> logError "Show stats")
        act

cnftCoinA :: FakeCoin
cnftCoinA = FakeCoin "aa"

initFunds :: Value
initFunds = mconcat [lovelaceValueOf 200_000_000, fakeValue cnftCoinA 1]

seabugActions :: Run ()
seabugActions = do
  -- Use the same slot config as is used onchain
  modify (\bch -> bch {bchConfig = (bchConfig bch) {bchConfigSlotConfig = def}})

  w1 <- newUser $ lovelaceValueOf 100_000_000 <> fakeValue cnftCoinA 1
  w2 <- newUser $ lovelaceValueOf 100_000_000

  void $
    mint w1 cnftCoinA 10_000_000
      >>= changePrice 8_000_000
      >>= marketplaceDeposit
      >>= marketplaceChangePrice 20_000_000
      >>= marketplaceBuy w2
      >>= marketplaceChangePrice 10_000_000
      >>= marketplaceRedeem
      >>= unstake

unstake :: NftData -> Run ()
unstake nftData = do
  box <- fromJust . find findCnft <$> scriptBoxAt lockValidator'
  utxos <- spend owner (singleton nftCS nftTN 1)
  now <- slotToBeginPOSIXTime def <$> currentSlot
  void
    . (sendTx <=< signTx owner <=< validateIn (range now))
    . addMintRedeemer policy' redeemer
    . mconcat
    $ [ spendBox lockValidator' (toBuiltinData $ Unstake (PaymentPubKeyHash owner) (nftId'price nft)) box
      , mintValue policy' nftVal
      , userSpend utxos
      , payToPubKey owner $ singleton cnftCS cnftTN 1 <> toValue minAdaTxOut
      ]
  where
    findCnft box = valueOf (txOutValue . txBoxOut $ box) cnftCS cnftTN == 1
    redeemer = BurnToken nft
    policy' = policy collection
    owner = unPaymentPubKeyHash . nftId'owner $ nft
    nftCS = scriptCurrencySymbol policy'
    nft = nftData'nftId nftData
    collection = nftData'nftCollection nftData
    nftTN = mkTokenName nft
    nftVal = singleton nftCS nftTN (-1)
    cnftCS = nftCollection'collectionNftCs collection
    cnftTN = nftId'collectionNftTn nft
    lockValidator' = lockValidator cnftCS 5 5
    range now = Interval (LowerBound (Finite now) True) (UpperBound PosInf False)

marketplaceBuy :: PubKeyHash -> NftData -> Run NftData
marketplaceBuy newOwner nftData = do
  box <- fromJust . find findNft <$> scriptBoxAt marketplaceValidator
  utxos <- spend newOwner (lovelaceValueOf . fromEnum . nftId'price . nftData'nftId $ nftData)
  void
    . (sendTx <=< signTx newOwner)
    . addMintRedeemer policy' redeemer
    . mconcat
    $ [ mintValue policy' (newNftVal <> oldNftVal)
      , payToScript marketplaceValidator (toBuiltinData ()) (newNftVal <> toValue minAdaTxOut)
      , spendBox marketplaceValidator (toBuiltinData Update) box
      , payWithDatumToPubKey oldOwner datum (lovelaceValueOf oldPrice)
      , userSpend utxos
      ]
      <> filterLowValue
        authorShare
        (payWithDatumToPubKey authorPkh datum (lovelaceValueOf authorShare))
      <> filterLowValue
        daoShare
        (payToScriptHash daoHash datum (lovelaceValueOf daoShare))
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    filterLowValue v t
      | v < getLovelace minAdaTxOut = mempty
      | otherwise = pure t
    getShare share = (oldPrice * share) `divide` 10000
    authorShare :: Integer = getShare (fromEnum . nftCollection'authorShare . nftData'nftCollection $ nftData)
    daoShare = getShare (fromEnum . nftCollection'daoShare . nftData'nftCollection $ nftData)
    datum = toBuiltinData (nftCS, oldNftTN)
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS oldNftTN == 1
    redeemer = ChangeOwner oldNft (PaymentPubKeyHash newOwner)
    policy' = policy (nftData'nftCollection nftData)
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'owner = PaymentPubKeyHash newOwner}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    oldOwner = unPaymentPubKeyHash . nftId'owner $ oldNft
    oldPrice = fromEnum . nftId'price $ oldNft
    authorPkh = unPaymentPubKeyHash . nftCollection'author . nftData'nftCollection $ nftData
    daoHash = nftCollection'daoScript . nftData'nftCollection $ nftData

marketplaceChangePrice :: Integer -> NftData -> Run NftData
marketplaceChangePrice newPrice nftData = do
  box <- fromJust . find findNft <$> scriptBoxAt marketplaceValidator
  void
    . (sendTx <=< signTx owner)
    . addMintRedeemer policy' redeemer
    . mconcat
    $ [ mintValue policy' (newNftVal <> oldNftVal)
      , payToScript marketplaceValidator (toBuiltinData ()) (newNftVal <> toValue minAdaTxOut)
      , spendBox marketplaceValidator (toBuiltinData Update) box
      ]
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS oldNftTN == 1
    redeemer = ChangePrice oldNft (toEnum newPrice)
    policy' = policy (nftData'nftCollection nftData)
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'price = toEnum newPrice}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    owner = unPaymentPubKeyHash . nftId'owner $ oldNft

marketplaceRedeem :: NftData -> Run NftData
marketplaceRedeem nftData = do
  box <- fromJust . find findNft <$> scriptBoxAt marketplaceValidator
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ spendBox marketplaceValidator (toBuiltinData . Redeem . nftData'nftId $ nftData) box
      , payToPubKey owner (nftVal <> toValue minAdaTxOut)
      ]
  pure nftData
  where
    findNft box = valueOf (txOutValue . txBoxOut $ box) nftCS nftTN == 1
    policy' = policy (nftData'nftCollection nftData)
    nftTN = mkTokenName . nftData'nftId $ nftData
    nftCS = scriptCurrencySymbol policy'
    nftVal = singleton nftCS nftTN 1
    owner = unPaymentPubKeyHash . nftId'owner . nftData'nftId $ nftData

marketplaceDeposit :: NftData -> Run NftData
marketplaceDeposit nftData = do
  utxos <- spend owner (singleton nftCS nftTN 1 <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx owner)
    . mconcat
    $ [ payToScript marketplaceValidator (toBuiltinData ()) (nftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure nftData
  where
    policy' = policy (nftData'nftCollection nftData)
    nftTN = mkTokenName . nftData'nftId $ nftData
    nftCS = scriptCurrencySymbol policy'
    nftVal = singleton nftCS nftTN 1
    owner = unPaymentPubKeyHash . nftId'owner . nftData'nftId $ nftData

changePrice :: Integer -> NftData -> Run NftData
changePrice newPrice nftData = do
  utxos <- spend owner (singleton nftCS oldNftTN 1 <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx owner)
    . addMintRedeemer policy' redeemer
    . mconcat
    $ [ mintValue policy' (newNftVal <> oldNftVal)
      , payToPubKey owner (newNftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure $ NftData (nftData'nftCollection nftData) newNft
  where
    redeemer = ChangePrice oldNft (toEnum newPrice)
    policy' = policy (nftData'nftCollection nftData)
    nftCS = scriptCurrencySymbol policy'
    oldNft = nftData'nftId nftData
    oldNftTN = mkTokenName oldNft
    oldNftVal = singleton nftCS oldNftTN (-1)
    newNft = oldNft {nftId'price = toEnum newPrice}
    newNftTN = mkTokenName newNft
    newNftVal = singleton nftCS newNftTN 1
    owner = unPaymentPubKeyHash . nftId'owner . nftData'nftId $ nftData

mint :: PubKeyHash -> FakeCoin -> Integer -> Run NftData
mint pkh cnftCoin price = do
  utxos <- spend pkh (cnftVal <> toValue minAdaTxOut)
  void
    . (sendTx <=< signTx pkh)
    . addMintRedeemer policy' redeemer
    . mconcat
    $ [ mintValue policy' nftVal
      , payToScript lockScript (toBuiltinData $ LockDatum nftCS 0 cnftTN) cnftVal
      , payToPubKey pkh (nftVal <> toValue minAdaTxOut)
      , userSpend utxos
      ]
  pure $ NftData collection nft
  where
    redeemer = MintToken nft
    lockScript = lockValidator cnftCS 5 5
    lockScriptHash = validatorHash lockScript
    marketplaceHash = validatorHash daoValidator
    collection =
      NftCollection
        cnftCS
        lockScriptHash
        (PaymentPubKeyHash pkh)
        (toEnum 10)
        marketplaceHash
        (toEnum 10)
    policy' = policy collection
    nft = NftId cnftTN (toEnum price) (PaymentPubKeyHash pkh)
    nftTN = mkTokenName nft
    nftCS = scriptCurrencySymbol policy'
    nftVal = singleton nftCS nftTN 1
    cnftTN = snd . unAssetClass . fakeCoin $ cnftCoin
    cnftCS = fst . unAssetClass . fakeCoin $ cnftCoin
    cnftVal = fakeValue cnftCoinA 1 <> toValue minAdaTxOut

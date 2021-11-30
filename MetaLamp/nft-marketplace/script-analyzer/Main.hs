{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DoAndIfThenElse #-}

import qualified Cardano.Api as C
import Data.Default (Default (..))
import Data.Monoid (Sum (..))
import Ledger.Index (ValidatorMode (..))
import Options.Applicative
import Plutus.Contract.Wallet (ExportTx (..))
import Plutus.Trace (Command (..), ScriptsConfig (..), showStats, writeScriptsTo)
import qualified Marketplace.Spec.Start as Marketplace
import qualified Marketplace.Spec.CreateNft as CreateNft
import qualified Marketplace.Spec.Bundles as Bundles
import qualified Marketplace.Spec.Sale as Sale
import qualified Marketplace.Spec.Auction as Auction
import           Control.Monad   (void)

writeWhat :: Command -> String
writeWhat (Scripts FullyAppliedValidators) = "scripts (fully applied)"
writeWhat (Scripts UnappliedValidators)    = "scripts (unapplied)"
writeWhat Transactions{}                   = "transactions"

pathParser :: Parser FilePath
pathParser = strArgument (metavar "SCRIPT_PATH" <> help "output path")

protocolParamsParser :: Parser FilePath
protocolParamsParser = strOption (long "protocol-parameters" <> short 'p' <> help "Path to protocol parameters JSON file" <> showDefault <> value "protocol-parameters.json")

networkIdParser :: Parser C.NetworkId
networkIdParser =
    let p = C.Testnet . C.NetworkMagic <$> option auto (long "network-magic" <> short 'n' <> help "Cardano network magic. If none is specified, mainnet addresses are generated.")
    in p <|> pure C.Mainnet

commandParser :: Parser Command
commandParser = hsubparser $ mconcat [scriptsParser, transactionsParser]

scriptsParser :: Mod CommandFields Command
scriptsParser =
    command "scripts" $
    info
        (Scripts <$> flag FullyAppliedValidators UnappliedValidators (long "unapplied-validators" <> short 'u' <> help "Write the unapplied validator scripts" <> showDefault))
        (fullDesc <> progDesc "Write fully applied validator scripts")

transactionsParser :: Mod CommandFields Command
transactionsParser =
    command "transactions" $
    info
        (Transactions <$> networkIdParser <*> protocolParamsParser)
        (fullDesc <> progDesc "Write partial transactions")

progParser :: ParserInfo ScriptsConfig
progParser =
    let p = ScriptsConfig <$> pathParser <*> commandParser
    in info
        (p <**> helper)
        (fullDesc
        <> progDesc "Run a number of emulator traces and write all validator scripts and/or partial transactions to SCRIPT_PATH"
        <> header "plutus-use-cases-scripts - extract validators and partial transactions from emulator traces"
        )

main :: IO ()
main = execParser progParser >>= writeScripts

writeScripts :: ScriptsConfig -> IO ()
writeScripts config = do
    putStrLn $ "Writing " <> writeWhat (scCommand config) <> " to: " <> scPath config
    (Sum size, exBudget) <- foldMap (uncurry3 (writeScriptsTo config))
        [ ("marketplace-start", Marketplace.startTrace, def)
        , ("create-nft-with-hidden-issuer", void CreateNft.createNftTrace, def)
        
        , ("bundle", void Bundles.bundleTrace, def)
        , ("unbundle", Bundles.unbundleTrace, def)
        
        , ("open-sale", void Sale.openSaleTrace, def)
        , ("close-sale", Sale.closeSaleTrace, def)
        , ("buy-item", Sale.buyItemTrace, def)

        , ("start-auction", void Auction.startAnAuctionTrace, def)
        , ("complete-auction", Auction.completeAnAuctionTrace, def)
        , ("bid-on-auction", void Auction.bidOnAuctionTrace, def)
        , ("buy-on-auction", Auction.buyOnAuctionTrace, def)
        , ("cancel-auction-with-bids", Auction.cancelAuctionWithBidsTrace, def)
        ]
    if size > 0 then
        putStrLn $ "Total " <> showStats size exBudget
    else pure ()

-- | `uncurry3` converts a curried function to a function on triples.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

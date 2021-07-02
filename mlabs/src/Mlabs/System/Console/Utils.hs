module Mlabs.System.Console.Utils(
    logAsciiLogo
  , logAction
  , logBalance
  , logMlabs
) where

import Control.Monad.IO.Class

import Prelude
import qualified Plutus.V1.Ledger.Value as Value
import qualified Data.ByteString.Char8 as Char8

import Mlabs.System.Console.PrettyLogger


logMlabs :: MonadIO m => m ()
logMlabs = logAsciiLogo (Vibrant Red) mlabs

mlabs :: String
mlabs =
  "                                                                           \n\
  \ ███╗   ███╗    ██╗      █████╗ ██████╗ ███████╗ \n\
  \ ████╗ ████║    ██║     ██╔══██╗██╔══██╗██╔════╝ \n\
  \ ██╔████╔██║    ██║     ███████║██████╔╝███████╗ \n\
  \ ██║╚██╔╝██║    ██║     ██╔══██║██╔══██╗╚════██║ \n\
  \ ██║ ╚═╝ ██║    ███████╗██║  ██║██████╔╝███████║ \n\
  \ ╚═╝     ╚═╝    ╚══════╝╚═╝  ╚═╝╚═════╝ ╚══════╝ "

logAsciiLogo :: MonadIO m => LogColor -> String -> m ()
logAsciiLogo color logo = do
  logNewLine
  logPrettyBgColor 40 color (Standard Black) logo
  logNewLine

logAction :: MonadIO m => String -> m ()
logAction str = logPrettyColorBold (Vibrant Green) (withNewLines $ str)

logBalance :: MonadIO m => String -> Value.Value -> m ()
logBalance wallet val = do
  logNewLine
  logPrettyBgColor 40 (Vibrant Cyan) (Standard Black) (wallet ++ " BALANCE")
  logNewLine
  logPrettyColor (Vibrant Cyan) (formatValue val)
  logNewLine

formatValue :: Value.Value -> String
formatValue v =
  unlines $ fmap formatTokenValue $
    filter ((/= 0) . (\(_,_,n) -> n)) $ Value.flattenValue v
  where
    formatTokenValue (_, name, value) =
      case name of
        "" -> (padRight ' ' 7 "Ada") ++ " " ++ (show value)
        (Value.TokenName n) -> (padRight ' ' 7 $ Char8.unpack n) ++ " " ++ (show value)


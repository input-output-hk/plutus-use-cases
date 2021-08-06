module Mlabs.System.Console.Utils (
  logAsciiLogo,
  logAction,
  logBalance,
  logMlabs,
) where

import Prelude

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString.Char8 qualified as Char8
import Plutus.V1.Ledger.Value qualified as Value
import System.Console.ANSI (Color (Black, Cyan, Green, Red))

import Mlabs.System.Console.PrettyLogger (LogColor (Standard, Vibrant))
import Mlabs.System.Console.PrettyLogger qualified as Pretty

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
  Pretty.logNewLine
  Pretty.logPrettyBgColor 40 color (Standard Black) logo
  Pretty.logNewLine

logAction :: MonadIO m => String -> m ()
logAction str = Pretty.logPrettyColorBold (Vibrant Green) (Pretty.withNewLines str)

logBalance :: MonadIO m => String -> Value.Value -> m ()
logBalance wallet val = do
  Pretty.logNewLine
  Pretty.logPrettyBgColor 40 (Vibrant Cyan) (Standard Black) (wallet ++ " BALANCE")
  Pretty.logNewLine
  Pretty.logPrettyColor (Vibrant Cyan) (formatValue val)
  Pretty.logNewLine

formatValue :: Value.Value -> String
formatValue v =
  unlines $
    fmap formatTokenValue $
      filter ((/= 0) . (\(_, _, n) -> n)) $ Value.flattenValue v
  where
    formatTokenValue (_, name, value) =
      case name of
        "" -> Pretty.padRight ' ' 7 "Ada" ++ " " ++ show value
        (Value.TokenName n) -> Pretty.padRight ' ' 7 $ Char8.unpack n ++ " " ++ show value

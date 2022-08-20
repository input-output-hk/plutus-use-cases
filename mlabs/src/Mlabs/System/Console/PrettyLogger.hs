{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Mlabs.System.Console.PrettyLogger (
  LogColor (..),
  LogStyle (..),
  logPretty,
  logPrettyColor,
  logPrettyBgColor,
  logPrettyColorBold,
  withNewLines,
  logNewLine,
  logDivider,
  padLeft,
  padRight,
) where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import System.Console.ANSI (
  Color,
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Background, Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  setSGR,
 )

-------------------------------------------------------------------------------

data LogStyle = LogStyle
  { bgColor :: LogColor
  , color :: LogColor
  , isBold :: Bool
  }

data LogColor
  = Vibrant Color
  | Standard Color
  | DefaultColor

defLogStyle :: LogStyle
defLogStyle =
  LogStyle {bgColor = DefaultColor, color = DefaultColor, isBold = False}

-------------------------------------------------------------------------------

logPretty :: MonadIO m => String -> m ()
logPretty = logPrettyStyled defLogStyle

logPrettyStyled :: MonadIO m => LogStyle -> String -> m ()
logPrettyStyled style string = liftIO $ do
  setSGR . foldMap ($ style) $
    [ getColorList . color
    , getBgColorList . bgColor
    , getConsoleIntensityList . isBold
    ]

  putStr string
  setSGR [Reset]
  where
    getColorList color = case color of
      Vibrant x -> [SetColor Foreground Vivid x]
      Standard x -> [SetColor Foreground Dull x]
      _ -> []
    getBgColorList bgColor = case bgColor of
      Vibrant x -> [SetColor Background Vivid x]
      Standard x -> [SetColor Background Dull x]
      _ -> []
    getConsoleIntensityList isBold =
      [SetConsoleIntensity BoldIntensity | isBold]

-- Convenience functions ------------------------------------------------------

logPrettyColor :: MonadIO m => LogColor -> String -> m ()
logPrettyColor color = logPrettyStyled defLogStyle {color = color}

logPrettyBgColor :: MonadIO m => Int -> LogColor -> LogColor -> String -> m ()
logPrettyBgColor minWidth bgColor color str =
  logPrettyStyled
    defLogStyle {bgColor = bgColor, color = color}
    (padRight ' ' minWidth str)

logPrettyColorBold :: MonadIO m => LogColor -> String -> m ()
logPrettyColorBold color =
  logPrettyStyled defLogStyle {color = color, isBold = True}

withNewLines :: String -> String
withNewLines string = "\n" ++ string ++ "\n"

logNewLine :: MonadIO m => m ()
logNewLine = logPretty "\n"

logDivider :: MonadIO m => m ()
logDivider =
  logPretty $
    "-----------------------------------------------------------"
      ++ "\n"

padLeft :: Char -> Int -> String -> String
padLeft char len txt = replicate (len - length txt) char <> txt

padRight :: Char -> Int -> String -> String
padRight char len txt = txt <> replicate (len - length txt) char

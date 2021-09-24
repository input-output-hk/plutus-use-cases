{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}

module Frontend.Widgets where

import Data.Map (Map)
import Data.Text (Text)
import Reflex.Dom.Core

toggleBtnUsability :: Bool -> Map Text Text
toggleBtnUsability True = ("class" =: "btn btn-lg btn-block btn-outline-primary")
toggleBtnUsability False = ("class" =: "btn btn-lg btn-block btn-secondary" <> "disabled" =: "true")

toggleSpinner :: Bool -> Map Text Text
toggleSpinner True = ("class" =: "d-none")
toggleSpinner False = ("class" =: "spinner-border spinner-border-sm" <> "role" =: "status" <> "aria-hidden" =: "true")

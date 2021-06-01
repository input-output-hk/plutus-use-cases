{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Frontend.App where

import Common.App
import Foreign.JavaScript.TH
import Reflex.Dom.Builder.Class
import Rhyolite.Frontend.App
import Language.Javascript.JSaddle.Types

type AppWidget t m =
  ( MonadRhyoliteFrontendWidget DefApp t m
  , HasDocument m
  , MonadJSM m
  , HasJSContext m
  )

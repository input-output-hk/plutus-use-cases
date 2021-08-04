module Error
  ( throwAffJaxError
  , throwMessage
  , throwDecodeError
  )
where

--------------------------------------------------------------------------------

import Prelude

import Data.Argonaut as A
import Affjax as AX
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw, message, name, stack)

--------------------------------------------------------------------------------

throwAffJaxError :: forall a. String -> AX.Error -> Aff a
throwAffJaxError m e = liftEffect $ throw $ m <> ": " <> AX.printError e

throwMessage :: forall a. String -> Error -> Aff a
throwMessage m e =
  liftEffect $ throw $
    joinWith ""
      [ m
      , name e
      , "\n"
      , message e
      , "\n"
      , case stack e of
          Just stack -> stack
          Nothing -> "stack not available"
      ]

throwDecodeError :: forall a. String -> String -> Aff a
throwDecodeError m e = liftEffect $ throw $ m <> ": " <> show e
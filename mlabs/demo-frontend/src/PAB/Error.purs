module Error
  ( throwRequestError
  , throwDecodeError
  ) where

--------------------------------------------------------------------------------

import Prelude

import Data.Argonaut as A
import Affjax as AX
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)

--------------------------------------------------------------------------------

throwRequestError :: forall a. String -> AX.Error -> Aff a
throwRequestError msg e = liftEffect $ throw $ msg <> ": " <> AX.printError e

throwDecodeError :: forall a. String -> A.JsonDecodeError -> Aff a
throwDecodeError msg e = do liftEffect $ throw $ msg <> ": " <> show e

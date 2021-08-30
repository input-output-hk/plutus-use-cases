module Utils.APIError where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data APIError
  = AjaxCallError String

derive instance genericAPIError :: Generic APIError _

instance showAPIError :: Show APIError where
  show = genericShow

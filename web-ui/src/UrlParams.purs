module UrlParams
  ( getUrlParam
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | Get a URL parameter by name
-- | Returns Nothing if the parameter is not present
getUrlParam :: String -> Effect (Maybe String)
getUrlParam name = toMaybe <$> getUrlParamImpl name

foreign import getUrlParamImpl :: String -> Effect (Nullable String)

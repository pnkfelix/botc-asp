module UrlParams
  ( getUrlParam
  , setUrlParam
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)

-- | Get a URL parameter by name
-- | Returns Nothing if the parameter is not present
getUrlParam :: String -> Effect (Maybe String)
getUrlParam name = toMaybe <$> getUrlParamImpl name

-- | Set a URL parameter (updates URL in-place without page reload)
-- | Uses history.replaceState so it doesn't add to browser history
setUrlParam :: String -> String -> Effect Unit
setUrlParam name value = setUrlParamImpl name value

foreign import getUrlParamImpl :: String -> Effect (Nullable String)
foreign import setUrlParamImpl :: String -> String -> Effect Unit

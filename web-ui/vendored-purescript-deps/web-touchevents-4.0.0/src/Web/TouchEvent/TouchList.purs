module Web.TouchEvent.TouchList
  ( TouchList
  , length
  , item
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Web.TouchEvent.Touch (Touch)

foreign import data TouchList :: Type

foreign import length :: TouchList -> Int

foreign import _item :: Fn2 Int TouchList (Nullable Touch)

item :: Int -> TouchList -> Maybe Touch
item i l = toMaybe $ runFn2 _item i l

module BuildInfo
  ( latestMergedPR
  , buildTimestamp
  ) where

import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)

-- | Latest merged PR number (extracted from git history at build time)
-- | Returns Nothing if no merged PR was found
foreign import latestMergedPRImpl :: Nullable Int

-- | Build timestamp as ISO string
foreign import buildTimestamp :: String

-- | Get the latest merged PR number
latestMergedPR :: Maybe Int
latestMergedPR = toMaybe latestMergedPRImpl

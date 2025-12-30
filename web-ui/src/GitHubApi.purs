module GitHubApi
  ( fetchLatestMergedPR
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)

-- | Foreign import for fetching latest merged PR number
foreign import fetchLatestMergedPRImpl :: String -> String -> Effect (Promise (Nullable Int))

-- | Fetch the latest merged PR number from GitHub
-- | Returns Nothing if the fetch fails or no merged PRs found
fetchLatestMergedPR :: String -> String -> Aff (Maybe Int)
fetchLatestMergedPR owner repo = do
  result <- toAffE (fetchLatestMergedPRImpl owner repo)
  pure $ toMaybe result

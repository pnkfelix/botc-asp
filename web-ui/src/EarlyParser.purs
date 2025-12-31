-- | Early parsing of players.lp and inst.lp to extract grimoire-relevant facts
-- | before running Clingo. This allows rendering a partial grimoire immediately.
module EarlyParser
  ( extractEarlyAtoms
  , extractFromFiles
  ) where

import Prelude

import Data.Array (catMaybes, filter, mapMaybe)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String as S
import Data.String.Regex (Regex, regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Either (hush)

-- | Extract early atoms from a Map of files (filepath -> content)
-- | Looks for players.lp and inst.lp to extract player/chair/received info
extractFromFiles :: Map String String -> Array String
extractFromFiles files =
  let
    playersContent = Map.lookup "players.lp" files
    instContent = Map.lookup "inst.lp" files
  in
    extractEarlyAtoms playersContent instContent

-- | Extract atoms from players.lp and inst.lp content
-- | Returns atoms in the same format as answer set output:
-- | - game_chair(player, position)
-- | - received(player, token)
extractEarlyAtoms :: Maybe String -> Maybe String -> Array String
extractEarlyAtoms playersContent instContent =
  let
    chairAtoms = case playersContent of
      Just content -> parseChairFacts content
      Nothing -> []
    receivedAtoms = case instContent of
      Just content -> parseReceivedFacts content
      Nothing -> []
  in
    chairAtoms <> receivedAtoms

-- | Parse chair/2 facts from players.lp content
-- | Format: chair(player, position).
parseChairFacts :: String -> Array String
parseChairFacts content =
  let
    -- Split into lines and parse each
    contentLines = split (Pattern "\n") content
    -- Match chair(player, N). pattern
    chairRegex = hush $ regex """chair\s*\(\s*(\w+)\s*,\s*(\d+)\s*\)\s*\.""" noFlags
  in
    case chairRegex of
      Just r -> mapMaybe (parseChairLine r) contentLines
      Nothing -> []

-- | Parse a single line for chair(player, position) fact
parseChairLine :: Regex -> String -> Maybe String
parseChairLine r line =
  case match r (trim line) of
    Just groups ->
      case groups of
        [_, Just player, Just posStr] ->
          case Int.fromString posStr of
            Just pos -> Just $ "game_chair(" <> player <> "," <> posStr <> ")"
            Nothing -> Nothing
        _ -> Nothing
    Nothing -> Nothing

-- | Parse received/2 facts from inst.lp content
-- | Format: received(player, token).
parseReceivedFacts :: String -> Array String
parseReceivedFacts content =
  let
    -- Split into lines and parse each
    contentLines = split (Pattern "\n") content
    -- Filter out commented lines first
    activeLines = filter (not <<< isCommentLine) contentLines
    -- Match received(player, token). pattern
    receivedRegex = hush $ regex """received\s*\(\s*(\w+)\s*,\s*(\w+)\s*\)\s*\.""" noFlags
  in
    case receivedRegex of
      Just r -> mapMaybe (parseReceivedLine r) activeLines
      Nothing -> []

-- | Check if a line is a comment (starts with %)
isCommentLine :: String -> Boolean
isCommentLine line =
  let trimmed = trim line
  in S.take 1 trimmed == "%"

-- | Parse a single line for received(player, token) fact
parseReceivedLine :: Regex -> String -> Maybe String
parseReceivedLine r line =
  case match r (trim line) of
    Just groups ->
      case groups of
        [_, Just player, Just token] ->
          Just $ "received(" <> player <> "," <> token <> ")"
        _ -> Nothing
    Nothing -> Nothing

-- | Early parsing of players.lp and inst.lp to extract grimoire-relevant facts
-- | before running Clingo. This allows rendering a partial grimoire immediately.
module EarlyParser
  ( extractEarlyAtoms
  , extractFromFiles
  , extractPlayerCount
  , parsePlayerCount
  , extractMinNights
  , parseMinNights
  , parseScript
  , ScriptInfo
  , extractScriptMetadata
  , extractAllScripts
  , getValidPlayerNames
  ) where

import Prelude

import Data.Array (filter, mapMaybe, fromFoldable, head, take)
import Data.Array.NonEmpty as NEA
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
-- | - bag(role) - from assert_drawn(role) facts
extractEarlyAtoms :: Maybe String -> Maybe String -> Array String
extractEarlyAtoms playersContent instContent =
  let
    chairAtoms = case playersContent of
      Just content -> parseChairFacts content
      Nothing -> []
    receivedAtoms = case instContent of
      Just content -> parseReceivedFacts content
      Nothing -> []
    bagAtoms = case instContent of
      Just content -> parseAssertDrawnFacts content
      Nothing -> []
    bluffAtoms = case instContent of
      Just content -> parseAssertBluffFacts content
      Nothing -> []
  in
    chairAtoms <> receivedAtoms <> bagAtoms <> bluffAtoms

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
      -- groups is NonEmptyArray: [fullMatch, player, posStr]
      case NEA.index groups 1, NEA.index groups 2 of
        Just (Just player), Just (Just posStr) ->
          Just $ "game_chair(" <> player <> "," <> posStr <> ")"
        _, _ -> Nothing
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
      -- groups is NonEmptyArray: [fullMatch, player, token]
      case NEA.index groups 1, NEA.index groups 2 of
        Just (Just player), Just (Just token) ->
          Just $ "received(" <> player <> "," <> token <> ")"
        _, _ -> Nothing
    Nothing -> Nothing

-- | Parse assert_drawn(role) facts from inst.lp content
-- | Returns bag(role) atoms so they display in pre-solve bag panel
parseAssertDrawnFacts :: String -> Array String
parseAssertDrawnFacts content =
  let
    -- Split into lines and parse each
    contentLines = split (Pattern "\n") content
    -- Filter out commented lines first
    activeLines = filter (not <<< isCommentLine) contentLines
    -- Match assert_drawn(role). pattern
    assertDrawnRegex = hush $ regex """assert_drawn\s*\(\s*(\w+)\s*\)\s*\.""" noFlags
  in
    case assertDrawnRegex of
      Just r -> mapMaybe (parseAssertDrawnLine r) activeLines
      Nothing -> []

-- | Parse a single line for assert_drawn(role) fact
parseAssertDrawnLine :: Regex -> String -> Maybe String
parseAssertDrawnLine r line =
  case match r (trim line) of
    Just groups ->
      -- groups is NonEmptyArray: [fullMatch, role]
      case NEA.index groups 1 of
        Just (Just role) ->
          -- Return as bag(role) so it shows in the bag panel
          Just $ "bag(" <> role <> ")"
        _ -> Nothing
    Nothing -> Nothing

-- | Parse assert_bluff(role) facts from inst.lp content
-- | Returns bluff(role) atoms so they display in pre-solve bluffs panel
parseAssertBluffFacts :: String -> Array String
parseAssertBluffFacts content =
  let
    -- Split into lines and parse each
    contentLines = split (Pattern "\n") content
    -- Filter out commented lines first
    activeLines = filter (not <<< isCommentLine) contentLines
    -- Match assert_bluff(role). pattern
    assertBluffRegex = hush $ regex """assert_bluff\s*\(\s*(\w+)\s*\)\s*\.""" noFlags
  in
    case assertBluffRegex of
      Just r -> mapMaybe (parseAssertBluffLine r) activeLines
      Nothing -> []

-- | Parse a single line for assert_bluff(role) fact
parseAssertBluffLine :: Regex -> String -> Maybe String
parseAssertBluffLine r line =
  case match r (trim line) of
    Just groups ->
      -- groups is NonEmptyArray: [fullMatch, role]
      case NEA.index groups 1 of
        Just (Just role) ->
          -- Return as bluff(role) so it shows in the bluffs panel
          Just $ "bluff(" <> role <> ")"
        _ -> Nothing
    Nothing -> Nothing

-- | Extract player_count from #const player_count = N. declarations
-- | Returns the first player_count found from any file in the Map
extractPlayerCount :: Map String String -> Maybe Int
extractPlayerCount files =
  let
    -- Check inst.lp first, then try other files
    instContent = Map.lookup "inst.lp" files
    allContents = fromFoldable $ Map.values files
  in case instContent of
    Just content ->
      case parsePlayerCount content of
        Just n -> Just n
        Nothing -> parsePlayerCountFromAny allContents
    Nothing -> parsePlayerCountFromAny allContents

-- | Parse #const player_count = N. from content
parsePlayerCount :: String -> Maybe Int
parsePlayerCount content =
  let
    contentLines = split (Pattern "\n") content
    playerCountRegex = hush $ regex """#const\s+player_count\s*=\s*(\d+)\s*\.""" noFlags
  in
    case playerCountRegex of
      Just r -> findFirstMatch r contentLines
      Nothing -> Nothing

-- | Find first matching line and extract the number
findFirstMatch :: Regex -> Array String -> Maybe Int
findFirstMatch r lines =
  head $ mapMaybe (parsePlayerCountLine r) lines

-- | Parse a single line for #const player_count = N
parsePlayerCountLine :: Regex -> String -> Maybe Int
parsePlayerCountLine r line =
  case match r (trim line) of
    Just groups ->
      case NEA.index groups 1 of
        Just (Just numStr) -> Int.fromString numStr
        _ -> Nothing
    Nothing -> Nothing

-- | Try to find player_count in any of the given contents
parsePlayerCountFromAny :: Array String -> Maybe Int
parsePlayerCountFromAny contents =
  head $ mapMaybe parsePlayerCount contents

-- | Extract min nights (needs_night) from inst.lp
-- | Returns the first needs_night value found from any file in the Map
extractMinNights :: Map String String -> Maybe Int
extractMinNights files =
  let
    -- Check inst.lp first, then try other files
    instContent = Map.lookup "inst.lp" files
    allContents = fromFoldable $ Map.values files
  in case instContent of
    Just content ->
      case parseMinNights content of
        Just n -> Just n
        Nothing -> parseMinNightsFromAny allContents
    Nothing -> parseMinNightsFromAny allContents

-- | Parse needs_night(N). from content
-- | Returns the highest value found (since multiple needs_night facts can exist)
parseMinNights :: String -> Maybe Int
parseMinNights content =
  let
    contentLines = split (Pattern "\n") content
    -- Filter out commented lines
    activeLines = filter (not <<< isCommentLine) contentLines
    minNightsRegex = hush $ regex """needs_night\s*\(\s*(\d+)\s*\)\s*\.""" noFlags
  in
    case minNightsRegex of
      Just r -> findMaxMatch r activeLines
      Nothing -> Nothing

-- | Find maximum matching needs_night value
findMaxMatch :: Regex -> Array String -> Maybe Int
findMaxMatch r lines =
  let values = mapMaybe (parseMinNightsLine r) lines
  in head $ filter (\_ -> true) values  -- Just get the first one for now

-- | Parse a single line for needs_night(N)
parseMinNightsLine :: Regex -> String -> Maybe Int
parseMinNightsLine r line =
  case match r (trim line) of
    Just groups ->
      case NEA.index groups 1 of
        Just (Just numStr) -> Int.fromString numStr
        _ -> Nothing
    Nothing -> Nothing

-- | Try to find needs_night in any of the given contents
parseMinNightsFromAny :: Array String -> Maybe Int
parseMinNightsFromAny contents =
  head $ mapMaybe parseMinNights contents

-- | Parse #include "SCRIPT.lp" from inst.lp content
-- | Returns the script id (e.g., "tb", "bmr", "snv")
-- | Only matches script files (tb.lp, bmr.lp, snv.lp, carousel.lp), not botc.lp or players.lp
parseScript :: String -> Maybe String
parseScript content =
  let
    contentLines = split (Pattern "\n") content
    -- Match #include "SCRIPT.lp". pattern where SCRIPT is tb, bmr, snv, or carousel
    scriptRegex = hush $ regex """#include\s+"(tb|bmr|snv|carousel)\.lp"\s*\.""" noFlags
  in
    case scriptRegex of
      Just r -> findFirstScriptMatch r contentLines
      Nothing -> Nothing

-- | Find first matching line and extract the script id
findFirstScriptMatch :: Regex -> Array String -> Maybe String
findFirstScriptMatch r lines =
  head $ mapMaybe (parseScriptLine r) lines

-- | Parse a single line for #include "SCRIPT.lp"
parseScriptLine :: Regex -> String -> Maybe String
parseScriptLine r line =
  case match r (trim line) of
    Just groups ->
      case NEA.index groups 1 of
        Just (Just scriptId) -> Just scriptId
        _ -> Nothing
    Nothing -> Nothing

-- | Script metadata extracted from comment headers
type ScriptInfo = { id :: String, name :: String }

-- | Extract script metadata from file content
-- | Looks for % @script-id: and % @script-name: comment lines
extractScriptMetadata :: String -> Maybe ScriptInfo
extractScriptMetadata content =
  let
    contentLines = split (Pattern "\n") content
    -- Parse @script-id and @script-name from first few lines
    scriptIdRegex = hush $ regex """^%\s*@script-id:\s*(.+)$""" noFlags
    scriptNameRegex = hush $ regex """^%\s*@script-name:\s*(.+)$""" noFlags
    maybeId = case scriptIdRegex of
      Just r -> findFirstMetadata r contentLines
      Nothing -> Nothing
    maybeName = case scriptNameRegex of
      Just r -> findFirstMetadata r contentLines
      Nothing -> Nothing
  in
    case maybeId, maybeName of
      Just id, Just name -> Just { id: trim id, name: trim name }
      _, _ -> Nothing

-- | Find first matching metadata line and extract the value
findFirstMetadata :: Regex -> Array String -> Maybe String
findFirstMetadata r lines =
  head $ mapMaybe (parseMetadataLine r) lines

-- | Parse a single line for metadata value
parseMetadataLine :: Regex -> String -> Maybe String
parseMetadataLine r line =
  case match r (trim line) of
    Just groups ->
      case NEA.index groups 1 of
        Just (Just value) -> Just value
        _ -> Nothing
    Nothing -> Nothing

-- | Extract all script info from a Map of files
-- | Scans all .lp files at root level for @script-id/@script-name metadata
extractAllScripts :: Map String String -> Array ScriptInfo
extractAllScripts files =
  let
    -- Get all file paths
    paths = fromFoldable $ Map.keys files
    -- Filter to root-level .lp files (no "/" in path, ends with .lp)
    rootLpFiles = filter isRootLpFile paths
    -- Extract metadata from each file
    scripts = mapMaybe (\path ->
      case Map.lookup path files of
        Just content -> extractScriptMetadata content
        Nothing -> Nothing
    ) rootLpFiles
  in
    scripts

-- | Check if a path is a root-level .lp file
isRootLpFile :: String -> Boolean
isRootLpFile path =
  not (S.contains (Pattern "/") path) && S.contains (Pattern ".lp") path

-- | Get valid player names based on player_count
-- | Returns array of player names for chairs 0 to (player_count - 1)
-- | Player ordering from players.lp: amanda(0), rob(1), taylor(2), courtney(3),
-- | steph(4), felix(5), neha(6), pratik(7), kunjal(8), cyrielle(9), logan(10),
-- | lou(11), kate(12), ivan(13), ricky(14), kyla(15)
getValidPlayerNames :: Int -> Array String
getValidPlayerNames count =
  let
    -- All player names in chair order (hardcoded for now, matches players.lp)
    allPlayers = ["amanda", "rob", "taylor", "courtney", "steph", "felix", "neha", "pratik",
                  "kunjal", "cyrielle", "logan", "lou", "kate", "ivan", "ricky", "kyla"]
  in
    take count allPlayers

-- | Parse and validate token placement constraints from LP files
-- | This module extracts never_applied_to/2 predicates and provides
-- | validation for drag operations.
module TokenConstraints
  ( NeverAppliedTo
  , parseNeverAppliedTo
  , validateReminderDrop
  , validateRoleDrop
  ) where

import Prelude

import Data.Array (filter, mapMaybe, any, head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim, indexOf)
import Data.String as String

-- | A never_applied_to constraint: token cannot be placed on a player with role
type NeverAppliedTo = { token :: String, role :: String }

-- | Parse all never_applied_to facts from LP file content
-- | Looks for patterns like: never_applied_to(ww_townsfolk, washerwoman).
parseNeverAppliedTo :: Map String String -> Array NeverAppliedTo
parseNeverAppliedTo files =
  let
    -- Combine all file contents
    allContent = Map.values files

    -- Parse each file
    parseFile :: String -> Array NeverAppliedTo
    parseFile content = mapMaybe parseLine (split (Pattern "\n") content)

    -- Parse a single line for never_applied_to(token, role).
    parseLine :: String -> Maybe NeverAppliedTo
    parseLine line =
      let
        trimmed = trim line
      in
        -- Check if line starts with never_applied_to(
        case indexOf (Pattern "never_applied_to(") trimmed of
          Just 0 -> parseNeverAppliedToLine trimmed
          _ -> Nothing
  in
    allContent >>= parseFile

-- | Parse a never_applied_to line like "never_applied_to(ww_townsfolk, washerwoman)."
-- | Uses simple string splitting instead of regex
parseNeverAppliedToLine :: String -> Maybe NeverAppliedTo
parseNeverAppliedToLine line = do
  -- Remove "never_applied_to(" prefix and find the closing ")"
  let withoutPrefix = String.drop 18 line  -- length of "never_applied_to("
  -- Find closing paren
  closeIdx <- indexOf (Pattern ")") withoutPrefix
  let argsStr = String.take closeIdx withoutPrefix
  -- Split on comma
  case split (Pattern ",") argsStr of
    [tokenPart, rolePart] ->
      Just { token: trim tokenPart, role: trim rolePart }
    _ -> Nothing

-- | Validate a reminder drop operation
-- | Returns Nothing if valid, or Just errorMessage if invalid
validateReminderDrop ::
  { token :: String, toPlayer :: String } ->
  Array NeverAppliedTo ->
  Array { name :: String, role :: String } ->  -- players with their roles
  Maybe String
validateReminderDrop { token, toPlayer } constraints players =
  let
    -- Find the role of toPlayer
    targetPlayerRole = findPlayerRole toPlayer players

    -- Check if this token has a constraint for this role
    violatesConstraint = case targetPlayerRole of
      Nothing -> false
      Just role -> any (\c -> c.token == token && c.role == role) constraints
  in
    if violatesConstraint
      then Just $ "Cannot place " <> token <> " on " <> toPlayer <>
                  " (who has the role that owns this token)"
      else Nothing

-- | Validate a role drop operation
-- | Returns Nothing if valid, or Just errorMessage if invalid
validateRoleDrop ::
  { role :: String, toPlayer :: String } ->
  Array NeverAppliedTo ->
  Array { token :: String, player :: String } ->  -- current reminders
  Maybe String
validateRoleDrop { role, toPlayer } constraints reminders =
  let
    -- Find reminders on toPlayer
    playerReminders = filter (\r -> r.player == toPlayer) reminders

    -- Check if any of those reminders have a never_applied_to constraint for this role
    violatingTokens = filter
      (\r -> any (\c -> c.token == r.token && c.role == role) constraints)
      playerReminders
  in
    case violatingTokens of
      [] -> Nothing
      _ -> Just $ "Cannot assign " <> role <> " to " <> toPlayer <>
                  " (has token(s) that cannot be on " <> role <> ")"

-- | Find a player's role from the player list
findPlayerRole :: String -> Array { name :: String, role :: String } -> Maybe String
findPlayerRole playerName players =
  head (filter (\p -> p.name == playerName) players) <#> _.role

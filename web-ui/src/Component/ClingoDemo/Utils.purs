-- | Utility functions for ClingoDemo component
-- | Extracted from ClingoDemo.purs to reduce file size
module Component.ClingoDemo.Utils where

import Prelude

import Data.Array (filter, fromFoldable, index, length, nub, slice, snoc)
import Data.Foldable (elem, foldl, intercalate)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim, contains, indexOf)
import Data.String as String
import EmbeddedPrograms as EP
import AnswerSetParser as AnswerSet
import Clingo as Clingo
import Component.ClingoDemo.Types (FileDiff, State, ResultDisplay(..), UndoEntry, TimingEntry)
import Component.TimelineGrimoire as TG
import Data.Set as Set
import EarlyParser as Early

-- | List of available file paths derived from embedded files
availableFiles :: Array String
availableFiles = fromFoldable $ Map.keys EP.lpFilesMap

-- | Get list of all unique directories from file paths
getDirectories :: Array String -> Array String
getDirectories paths =
  nub $ paths >>= \path ->
    case getParentDir path of
      "" -> []
      dir -> [dir]

-- | Get parent directory of a path (empty string for root files)
getParentDir :: String -> String
getParentDir path =
  let parts = filter (_ /= "") $ split (Pattern "/") path
  in if length parts <= 1
     then ""
     else intercalate "/" (slice 0 (length parts - 1) parts)

-- | Get just the filename from a path
getFileName :: String -> String
getFileName path =
  let parts = filter (_ /= "") $ split (Pattern "/") path
  in fromMaybe path $ index parts (length parts - 1)

-- | Check if a file is in a directory (directly, not nested)
isInDirectory :: String -> String -> Boolean
isInDirectory dir path =
  getParentDir path == dir

-- | Check if a file is at root level (no parent directory)
isRootFile :: String -> Boolean
isRootFile path = getParentDir path == ""

-- | Join a directory and filename into a path
joinPath :: String -> String -> String
joinPath "" filename = filename
joinPath dir filename = dir <> "/" <> filename

-- | Resolve an include path: try as-is first, then relative to current directory
resolveIncludePath :: String -> String -> Map.Map String String -> String
resolveIncludePath currentDir includePath files =
  -- Try the path as-is first (relative to root)
  if Map.member includePath files
    then includePath
    -- Try relative to current directory
    else
      let relativePath = joinPath currentDir includePath
      in if Map.member relativePath files
           then relativePath
           else includePath  -- Return original path even if not found

-- | Update player_count in inst.lp content
-- | Replaces the line "#const player_count = N." with the new value
updatePlayerCount :: Int -> String -> String
updatePlayerCount newCount content =
  let
    contentLines = split (Pattern "\n") content
    updatedLines = map updateLine contentLines
  in
    intercalate "\n" updatedLines
  where
    updateLine line =
      let trimmed = trim line
      in if String.take 20 trimmed == "#const player_count "
           then "#const player_count = " <> show newCount <> "."
           else line

-- | Valid script identifiers
validScripts :: Array String
validScripts = ["tb", "bmr", "snv", "carousel"]

-- | Check if a script identifier is valid
isValidScript :: String -> Boolean
isValidScript scriptId = elem scriptId validScripts

-- | Update script include in inst.lp content
-- | Replaces the line '#include "SCRIPT.lp".' with the new script
updateScript :: String -> String -> String
updateScript newScript content =
  if not (isValidScript newScript) then content
  else
    let
      contentLines = split (Pattern "\n") content
      updatedLines = map updateLine contentLines
    in
      intercalate "\n" updatedLines
  where
    updateLine line =
      let trimmed = trim line
      in if isScriptInclude trimmed
           then "#include \"" <> newScript <> ".lp\"."
           else line
    -- Check if line is a script include (tb, bmr, snv, or carousel)
    isScriptInclude str =
      String.take 10 str == "#include \"" &&
      (String.contains (Pattern "\"tb.lp\"") str ||
       String.contains (Pattern "\"bmr.lp\"") str ||
       String.contains (Pattern "\"snv.lp\"") str ||
       String.contains (Pattern "\"carousel.lp\"") str)

-- | Strip ASP comments from a string (lines starting with %)
-- | Also strips empty lines and trims whitespace
-- | Returns an array of non-comment lines
stripComments :: String -> Array String
stripComments content =
  let
    allLines = split (Pattern "\n") content
    -- Filter out comment lines and empty lines, trim whitespace
    nonCommentLines = filter isNonComment allLines
  in
    map trim nonCommentLines
  where
    isNonComment line =
      let trimmed = trim line
      in trimmed /= "" && String.take 1 trimmed /= "%"

-- | Compute file diffs comparing current files to original embedded files
-- | Uses simple line comparison (comments stripped)
-- | Returns both a summary string and detailed per-file diffs for the modal
computeFileDiff :: Map.Map String String -> { summary :: String, fileDiffs :: Array FileDiff }
computeFileDiff currentFiles =
  let
    -- Get all file paths to compare
    allPaths = fromFoldable $ Map.keys currentFiles
    -- Compare each file and collect diffs (with comments stripped)
    fileDiffsAndDescs = allPaths >>= \path ->
      let
        current = fromMaybe "" $ Map.lookup path currentFiles
        original = fromMaybe "" $ Map.lookup path EP.lpFilesMap
        -- Strip comments before comparing
        currentStripped = stripComments current
        originalStripped = stripComments original
      in
        if currentStripped == originalStripped
          then []
          else
            let
              currentLen = length currentStripped
              originalLen = length originalStripped
              -- Simple line count difference
              added = if currentLen > originalLen then currentLen - originalLen else 0
              removed = if originalLen > currentLen then originalLen - currentLen else 0
              diffDesc = if added > 0 && removed > 0
                then "+" <> show added <> "/-" <> show removed
                else if added > 0
                  then "+" <> show added
                  else if removed > 0
                    then "-" <> show removed
                    else "modified"
              fileDiff = { fileName: getFileName path
                         , originalLines: originalStripped
                         , currentLines: currentStripped
                         }
            in
              [{ desc: getFileName path <> ": " <> diffDesc, diff: fileDiff }]
    -- Extract summaries and diffs
    summaries = map _.desc fileDiffsAndDescs
    diffs = map _.diff fileDiffsAndDescs
  in
    { summary: if length summaries == 0 then "No changes" else intercalate ", " summaries
    , fileDiffs: diffs
    }

-- | Get files to show in tabs: root files + current file if it's in a subdirectory
getVisibleTabs :: String -> Array String
getVisibleTabs currentFile =
  let
    rootFiles = filter isRootFile availableFiles
  in
    if isRootFile currentFile
      then rootFiles
      else rootFiles <> [currentFile]

-- | Initial state with embedded .lp file contents
initialState :: State
initialState =
  { files: EP.lpFilesMap  -- Use Map for efficient lookups
  , currentFile: "inst.lp"  -- Start with instance file selected
  , showFileDirectory: false
  , expandedDirs: Set.empty  -- All directories collapsed initially
  , modelLimit: ""  -- Empty = default to 5 models, 0 = all models
  , result: Nothing
  , isLoading: false
  , isInitialized: false
  , selectedModelIndex: 0  -- First model selected by default
  , answerSetPage: 0       -- First page of answer sets
  , showPredicateList: false
  , selectedPredicate: Nothing
  , outputFilter: ""       -- No filtering by default
  , scrollNotification: Nothing  -- No notification initially
  , undoStack: []          -- Empty undo stack
  , redoStack: []          -- Empty redo stack
  , navigateIncludeTarget: Nothing  -- No navigation dialog initially
  , timingHistory: []      -- No timing history initially
  , nextRunIndex: 1        -- Start run indexing at 1
  , selectedTimingEntry: Nothing  -- No diff modal initially
  }

-- | Get current file content from state
getCurrentFileContent :: State -> String
getCurrentFileContent state = fromMaybe "" $ Map.lookup state.currentFile state.files

-- | Build sources array from files Map for parsing
getSources :: State -> Array { name :: String, content :: String }
getSources state = map (\path -> { name: path, content: fromMaybe "" $ Map.lookup path state.files }) availableFiles

-- | Get atoms for the grimoire - either from answer set or early parsing
-- | Returns a tuple of (atoms, isEarly) where isEarly indicates if using early parsing
getGrimoireAtoms :: State -> { atoms :: Array String, isEarly :: Boolean, modelInfo :: String }
getGrimoireAtoms state =
  case state.result of
    Just (ResultSuccess answerSets) ->
      case index answerSets state.selectedModelIndex of
        Just atoms ->
          let totalCount = length answerSets
              modelInfo = if totalCount > 1
                then " (Model " <> show (state.selectedModelIndex + 1) <> " of " <> show totalCount <> ")"
                else ""
          in { atoms, isEarly: false, modelInfo }
        Nothing ->
          -- Selected index out of range, fall back to early parsing
          getEarlyAtomsFiltered state.files
    _ ->
      -- No result or error/unsat, use early parsing
      getEarlyAtomsFiltered state.files

-- | Get script roles by resolving includes from the current files
-- | This extracts role definitions from the resolved ASP program
getScriptRoles :: Map.Map String String -> Clingo.ScriptRoles
getScriptRoles files =
  let
    -- Use inst.lp as the entry point (same as RunClingo)
    entryFile = "inst.lp"
    entryProgram = fromMaybe "" $ Map.lookup entryFile files
    resolver filename = Map.lookup filename files
    -- Resolve all #include directives to get the full program
    fullProgram = Clingo.resolveIncludesWithPath entryProgram entryFile resolver
  in
    Clingo.extractScriptRoles fullProgram

-- | Get full input for TimelineGrimoire component (atoms + script roles)
getGrimoireInput :: State -> TG.Input
getGrimoireInput state =
  let
    atomData = getGrimoireAtoms state
    scriptRoles = getScriptRoles state.files
  in
    { atoms: atomData.atoms
    , scriptRoles: scriptRoles
    }

-- | Get early atoms filtered by player_count
-- | Only includes game_chair atoms where chair position < player_count (0-indexed)
getEarlyAtomsFiltered :: Map.Map String String -> { atoms :: Array String, isEarly :: Boolean, modelInfo :: String }
getEarlyAtomsFiltered files =
  let
    allAtoms = Early.extractFromFiles files
    playerCount = Early.extractPlayerCount files
    filteredAtoms = case playerCount of
      Just n -> filter (isAtomWithinPlayerCount n) allAtoms
      Nothing -> allAtoms  -- No player_count found, show all
  in
    { atoms: filteredAtoms, isEarly: true, modelInfo: "" }

-- | Check if an atom should be included based on player_count
-- | For game_chair(player, N), only include if N < player_count (0-indexed chairs)
-- | All other atoms are included
isAtomWithinPlayerCount :: Int -> String -> Boolean
isAtomWithinPlayerCount maxPlayers atom =
  case parseChairPosition atom of
    Just pos -> pos < maxPlayers
    Nothing -> true  -- Non-chair atoms are always included

-- | Parse the chair position from a game_chair(player, N) atom
parseChairPosition :: String -> Maybe Int
parseChairPosition atom =
  -- Pattern: game_chair(player,N)
  if String.take 11 atom == "game_chair("
    then
      -- Extract the number after the comma
      let
        inner = String.drop 11 atom  -- Remove "game_chair("
        -- Find the comma and extract the number after it
        parts = String.split (String.Pattern ",") inner
      in case index parts 1 of
        Just numPart ->
          -- Remove trailing ")" and parse
          let cleaned = String.take (String.length numPart - 1) numPart
          in Int.fromString (trim cleaned)
        Nothing -> Nothing
    else Nothing

-- | Get description for a file based on its path
getFileDescription :: String -> String
getFileDescription path = case path of
  "inst.lp" -> "Instance/scenario configuration"
  "botc.lp" -> "Core game rules"
  "tb.lp" -> "Trouble Brewing script"
  "players.lp" -> "Player names and seating"
  "types.lp" -> "Type validation rules"
  "bmr.lp" -> "Bad Moon Rising script"
  "carousel.lp" -> "Carousel script"
  _ ->
    -- Try to infer from filename pattern
    let name = getFileName path
    in if String.take 4 name == "sat_"
       then "Test: should be satisfiable"
       else if String.take 6 name == "unsat_"
         then "Test: should be unsatisfiable"
         else if name == "base.lp"
           then "Base rules for this directory"
           else ""

-- | Convert a source file name to textarea ID (now just one editor)
sourceFileToTextareaId :: String -> Maybe String
sourceFileToTextareaId _ = Just "editor-textarea"

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

-- | Format a TimePoint for ASP syntax
formatTimePointForASP :: AnswerSet.TimePoint -> String
formatTimePointForASP AnswerSet.Setup = "setup"
formatTimePointForASP (AnswerSet.Night n r s) = "night(" <> show n <> ", " <> show r <> ", " <> show s <> ")"
formatTimePointForASP (AnswerSet.Dawn n) = "dawn(" <> show n <> ")"
formatTimePointForASP (AnswerSet.Day n phase) = "day(" <> show n <> ", " <> phase <> ")"
formatTimePointForASP (AnswerSet.UnknownTime s) = s

-- | Modify inst.lp to add a new reminder constraint and comment out conflicting one
-- | Takes token, fromPlayer, toPlayer for descriptive comments
modifyInstLpForReminder :: String -> String -> String -> String -> String -> String -> String
modifyInstLpForReminder content oldPattern newConstraint token fromPlayer toPlayer =
  let
    -- Split content into lines
    contentLines = String.split (String.Pattern "\n") content
    -- Context for commented-out line: what was moved and where
    commentOutContext = "moved " <> token <> " to " <> toPlayer
    -- Comment out any existing line matching the old pattern
    modifiedLines = map (commentOutIfMatchesWithContext oldPattern commentOutContext) contentLines
    -- Check if the new constraint already exists
    hasNewConstraint = foldl (\acc line -> acc || trim line == trim newConstraint) false modifiedLines
    -- Create descriptive comment for the new line
    addedComment = "% Moved " <> token <> " from " <> fromPlayer <> " to " <> toPlayer <> " (added by drag)"
    -- Add the new constraint at the end if not already present
    finalLines = if hasNewConstraint
                   then modifiedLines
                   else modifiedLines <> ["", addedComment, newConstraint]
  in
    intercalate "\n" finalLines

-- | Check if a line is an assert_received constraint for a specific player (any role)
-- | Returns true if the trimmed line matches pattern: assert_received(<player>, <any_role>).
isAssertReceivedForPlayer :: String -> String -> Boolean
isAssertReceivedForPlayer player line =
  let
    trimmedLine = trim line
    -- Pattern: assert_received(<player>,
    prefix = "assert_received(" <> player <> ", "
    -- Check if line starts with the prefix (not commented out)
    startsWithPrefix = indexOf (Pattern prefix) trimmedLine == Just 0
    -- Check if line ends with ). to confirm it's a complete constraint
    endsCorrectly = contains (Pattern ").") trimmedLine
  in
    -- Must start with assert_received(<player>, and end with ).
    startsWithPrefix && endsCorrectly

-- | Check if a line is an assert_received constraint for a specific role (any player)
-- | Returns true if the trimmed line matches pattern: assert_received(<any_player>, <role>).
isAssertReceivedForRole :: String -> String -> Boolean
isAssertReceivedForRole role line =
  let
    trimmedLine = trim line
    prefix = "assert_received("
    suffix = ", " <> role <> ")."
    -- Check if line starts with the prefix (not commented out)
    startsWithPrefix = indexOf (Pattern prefix) trimmedLine == Just 0
    -- Check if line contains the role suffix
    endsWithSuffix = contains (Pattern suffix) trimmedLine
  in
    -- Must start with assert_received( (not commented) and contain the role suffix
    startsWithPrefix && endsWithSuffix

-- | Comment out a line if it matches an assert_received constraint for either:
-- | 1. The given player (any role) - because a player can only have one role
-- | 2. The given role (any player) - because a role can only be assigned to one player
commentOutIfPlayerOrRoleConstraint :: String -> String -> String -> String -> String
commentOutIfPlayerOrRoleConstraint player role contextSuffix line =
  if isAssertReceivedForPlayer player line || isAssertReceivedForRole role line
    then "% " <> line <> "  % commented out by drag: " <> contextSuffix
    else line

-- | Comment out a line if it exactly matches the pattern (case-sensitive, trimmed comparison)
-- | Used for reminder tokens where exact player match is needed
-- | Takes additional context to describe what was moved and where
commentOutIfMatchesWithContext :: String -> String -> String -> String
commentOutIfMatchesWithContext patternStr contextSuffix line =
  let
    trimmedLine = trim line
    trimmedPattern = trim patternStr
    firstChar = String.take 1 trimmedLine
  in
    if trimmedLine == trimmedPattern && firstChar /= "%"
      then "% " <> line <> "  % commented out by drag: " <> contextSuffix
      else line

-- | Convert a TimePoint to an integer time index for assigned/3 predicate
-- Setup and Night 1 (any phase) map to 0 (initial assignment)
-- Other nights/dawns/days map to their number
timePointToAssignedTime :: AnswerSet.TimePoint -> Int
timePointToAssignedTime AnswerSet.Setup = 0
timePointToAssignedTime (AnswerSet.Night 1 _ _) = 0
timePointToAssignedTime (AnswerSet.Night n _ _) = n
timePointToAssignedTime (AnswerSet.Dawn n) = n
timePointToAssignedTime (AnswerSet.Day n _) = n
timePointToAssignedTime (AnswerSet.UnknownTime _) = 0

-- | Modify inst.lp to add a new role assignment constraint and comment out conflicting ones
-- | Comments out ALL assert_received constraints that conflict with this assignment:
-- | 1. Any role previously assigned to toPlayer (a player can only have one role)
-- | 2. This role previously assigned to any player (a role can only be on one player)
-- | Takes role, fromPlayer, toPlayer for descriptive comments
modifyInstLpForRole :: String -> String -> String -> String -> String -> String -> String
modifyInstLpForRole content _oldPattern newConstraint role fromPlayer toPlayer =
  let
    -- Split content into lines
    contentLines = String.split (String.Pattern "\n") content
    -- Context for commented-out line: what was moved and where
    commentOutContext = "assigned " <> role <> " to " <> toPlayer
    -- Comment out ALL conflicting assert_received constraints:
    -- 1. Any role on toPlayer (player can only have one role)
    -- 2. This role on any player (role can only be on one player)
    modifiedLines = map (commentOutIfPlayerOrRoleConstraint toPlayer role commentOutContext) contentLines
    -- Check if the new constraint already exists
    hasNewConstraint = foldl (\acc line -> acc || trim line == trim newConstraint) false modifiedLines
    -- Create descriptive comment for the new line
    addedComment = "% Moved " <> role <> " from " <> fromPlayer <> " to " <> toPlayer <> " (added by drag)"
    -- Add the new constraint at the end if not already present
    finalLines = if hasNewConstraint
                   then modifiedLines
                   else modifiedLines <> ["", addedComment, newConstraint]
  in
    intercalate "\n" finalLines

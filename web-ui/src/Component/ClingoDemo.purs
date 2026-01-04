-- | ClingoDemo component - main Halogen component for the ASP explorer
-- | Types, utilities, and render functions are extracted to submodules to reduce file size
module Component.ClingoDemo where

import Prelude

import AnswerSetParser as AnswerSet
import Clingo as Clingo
import Component.TimelineGrimoire as TG
import Data.Array (index, length, snoc, unsnoc)
import Data.Array as Array
import Data.Foldable (class Foldable, elem)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Set as Set
import Data.String (trim)
import Data.String as String
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import EarlyParser (parsePlayerCount, parseScript)
import Halogen as H
import TextareaUtils as TU
import TokenConstraints as TC
import UrlParams as UP
import Web.UIEvent.MouseEvent (toEvent)

-- Re-export types and render function for backwards compatibility
import Component.ClingoDemo.Types (Action(..), ResultDisplay(..), Slots, State, TimingEntry, UndoEntry, answerSetPageSize)
import Component.ClingoDemo.Utils (availableFiles, commentOutConstraint, computeFileDiff, extractWitnesses, formatTimePointForASP, getCurrentFileContent, getParentDir, initialState, isValidScript, modifyInstLpForReminder, modifyInstLpForRole, resolveIncludePath, updatePlayerCount, updateScript)
import Component.ClingoDemo.Render (render)

-- | The Halogen component
component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: \_ -> initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

-- | Handle component actions
handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize clingo-wasm (relative path works locally and on GitHub Pages)
    H.liftAff $ Clingo.init "./clingo.wasm"
    -- Check for URL parameters and update inst.lp if present
    maybePlayerCount <- liftEffect $ UP.getUrlParam "player_count"
    maybeScript <- liftEffect $ UP.getUrlParam "script"
    -- Apply URL parameter updates to inst.lp
    let updateFromParams :: String -> String
        updateFromParams content =
          let
            -- Apply player_count update if present
            withPlayerCount = case maybePlayerCount >>= Int.fromString of
              Just n -> updatePlayerCount n content
              Nothing -> content
            -- Apply script update if present and valid
            withScript = case maybeScript of
              Just scriptId | isValidScript scriptId -> updateScript scriptId withPlayerCount
              _ -> withPlayerCount
          in withScript
    H.modify_ \s ->
      let
        instContent = fromMaybe "" $ Map.lookup "inst.lp" s.files
        updatedContent = updateFromParams instContent
        updatedFiles = Map.insert "inst.lp" updatedContent s.files
      in s { files = updatedFiles, isInitialized = true }
    -- Initialize syntax highlighting for the initial file
    state <- H.get
    let content = fromMaybe "" $ Map.lookup state.currentFile state.files
    liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" content

  SelectFile fileName -> do
    H.modify_ \s -> s { currentFile = fileName, showFileDirectory = false }
    -- Update syntax highlighting for the newly selected file
    state <- H.get
    let content = fromMaybe "" $ Map.lookup state.currentFile state.files
    liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" content

  SetFileContent content -> do
    state <- H.get
    H.modify_ \s -> s { files = Map.insert s.currentFile content s.files }
    -- If editing inst.lp, sync player_count and script to URL for persistence across reloads
    when (state.currentFile == "inst.lp") do
      case parsePlayerCount content of
        Just n -> liftEffect $ UP.setUrlParam "player_count" (show n)
        Nothing -> pure unit
      case parseScript content of
        Just scriptId -> liftEffect $ UP.setUrlParam "script" scriptId
        Nothing -> pure unit
    -- Update syntax highlighting as content changes
    liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" content

  ToggleFileDirectory ->
    H.modify_ \s -> s { showFileDirectory = not s.showFileDirectory }

  ToggleDirectory dir ->
    H.modify_ \s ->
      let newExpanded = if Set.member dir s.expandedDirs
            then Set.delete dir s.expandedDirs
            else Set.insert dir s.expandedDirs
      in s { expandedDirs = newExpanded }

  SetModelLimit limit ->
    H.modify_ \s -> s { modelLimit = limit }

  SetOutputFilter filterExpr ->
    H.modify_ \s -> s { outputFilter = filterExpr }

  RunClingo -> do
    -- Keep previous result visible during loading (preserves TimelineGrimoire state)
    H.modify_ \s -> s { isLoading = true, selectedModelIndex = 0, answerSetPage = 0 }
    state <- H.get
    -- Parse model limit (empty = 5 default, 0 = all models)
    let numModels = case trim state.modelLimit of
          "" -> 5  -- Default to 5 models when field is empty
          s -> fromMaybe 5 $ Int.fromString s
    -- Compute the diff description before running (for timing table)
    let diffResult = computeFileDiff state.files
    -- Build file resolver for #include directives using the virtual filesystem
    let resolver filename = Map.lookup filename state.files
    -- Get the current file's content (the entry point that #includes other files)
    let entryFile = state.currentFile
    let entryProgram = fromMaybe "" $ Map.lookup entryFile state.files
    -- Resolve #include directives recursively, using path-aware resolution
    -- This follows Clingo's behavior: try CWD first, then relative to including file
    let fullProgram = Clingo.resolveIncludesWithPath entryProgram entryFile resolver
    result <- H.liftAff $ Clingo.run fullProgram numModels
    -- Extract timing, model info, and display from result
    let { display, timing, modelInfo } = case result of
          Clingo.Satisfiable res ->
            { display: ResultSuccess $ extractWitnesses res
            , timing: Just res."Time"
            , modelInfo: Just { count: res."Models"."Number", more: res."Models"."More" == "yes" }
            }
          Clingo.OptimumFound res ->
            { display: ResultSuccess $ extractWitnesses res
            , timing: Just res."Time"
            , modelInfo: Just { count: res."Models"."Number", more: res."Models"."More" == "yes" }
            }
          Clingo.Unsatisfiable res ->
            { display: ResultUnsat
            , timing: Just res."Time"
            , modelInfo: Just { count: 0, more: false }
            }
          Clingo.Unknown res ->
            { display: ResultError "Result unknown"
            , timing: Just res."Time"
            , modelInfo: Just { count: res."Models"."Number", more: res."Models"."More" == "yes" }
            }
          Clingo.Error err ->
            { display: ResultError err
            , timing: Nothing
            , modelInfo: Nothing
            }
    -- Create timing entry if we have timing data
    let newEntry = case timing, modelInfo of
          Just t, Just m -> Just
            { diffSummary: diffResult.summary
            , fileDiffs: diffResult.fileDiffs
            , modelLimit: numModels
            , actualModelCount: m.count
            , moreModels: m.more
            , totalTime: t."Total"
            , solveTime: t."Solve"
            , runIndex: state.nextRunIndex
            }
          _, _ -> Nothing
    -- Update state with result and timing history
    H.modify_ \s -> s
      { isLoading = false
      , result = Just display
      , timingHistory = case newEntry of
          Just entry -> snoc s.timingHistory entry
          Nothing -> s.timingHistory
      , nextRunIndex = s.nextRunIndex + 1
      }

  CancelSolve -> do
    -- Restart the worker to cancel the current solve
    H.liftAff $ Clingo.restart "./clingo.wasm"
    H.modify_ \s -> s { isLoading = false, result = Just (ResultError "Solve cancelled by user") }

  SelectModel idx ->
    H.modify_ \s -> s { selectedModelIndex = idx }

  PrevAnswerSetPage ->
    H.modify_ \s -> s { answerSetPage = max 0 (s.answerSetPage - 1) }

  NextAnswerSetPage -> do
    state <- H.get
    let maxPage = case state.result of
          Just (ResultSuccess answerSets) ->
            (length answerSets - 1) / answerSetPageSize
          _ -> 0
    H.modify_ \s -> s { answerSetPage = min maxPage (s.answerSetPage + 1) }

  TogglePredicateList ->
    H.modify_ \s -> s { showPredicateList = not s.showPredicateList }

  SelectPredicate pred ->
    H.modify_ \s -> s { selectedPredicate = Just pred }

  ClosePredicateModal ->
    H.modify_ \s -> s { selectedPredicate = Nothing }

  JumpToReference sourceFile lineNumber -> do
    -- Close modal, predicate panel, and switch to the source file
    H.modify_ \s -> s { selectedPredicate = Nothing, showPredicateList = false, currentFile = sourceFile }
    -- Scroll to the line in the editor textarea
    liftEffect $ TU.scrollToLine "editor-textarea" lineNumber

  HandleTimelineEvent output -> case output of
    TG.TimelineEventClicked { sourceAtom, predicateName } -> do
      -- Scroll to the atom in the currently selected answer set
      if sourceAtom /= ""
        then do
          state <- H.get
          -- Calculate which child index corresponds to the selected model on the current page
          let pageStart = state.answerSetPage * answerSetPageSize
          let pageEnd = pageStart + answerSetPageSize
          let selectedIdx = state.selectedModelIndex
          -- Only highlight if the selected model is visible on the current page
          if selectedIdx >= pageStart && selectedIdx < pageEnd
            then do
              let childIndex = selectedIdx - pageStart
              found <- liftEffect $ TU.scrollToTextInChild "answer-set-display" childIndex sourceAtom
              -- Show notification if atom not found in answer set
              when (not found) do
                H.modify_ \s -> s { scrollNotification = Just $ "Could not find '" <> sourceAtom <> "' in the answer set" }
                -- Clear notification after 3 seconds
                _ <- H.fork do
                  H.liftAff $ Aff.delay (Milliseconds 3000.0)
                  H.modify_ \s -> s { scrollNotification = Nothing }
                pure unit
            else do
              -- Notify user that the model is not on the current page
              H.modify_ \s -> s { scrollNotification = Just $ "Model " <> show (selectedIdx + 1) <> " is not on the current page" }
              _ <- H.fork do
                H.liftAff $ Aff.delay (Milliseconds 3000.0)
                H.modify_ \s -> s { scrollNotification = Nothing }
              pure unit
        else do
          -- Empty sourceAtom - this shouldn't normally happen
          H.modify_ \s -> s { scrollNotification = Just $ "No source atom for " <> predicateName <> " event" }
          _ <- H.fork do
            H.liftAff $ Aff.delay (Milliseconds 3000.0)
            H.modify_ \s -> s { scrollNotification = Nothing }
          pure unit
    TG.ReminderMoved { token, fromPlayer, toPlayer, time } -> do
      -- Validate the drop using never_applied_to constraints
      state <- H.get
      let constraints = TC.parseNeverAppliedTo state.files
      -- Get current game state to check player roles
      let maybeGameState = case state.result of
            Just (ResultSuccess answerSets) ->
              case index answerSets state.selectedModelIndex of
                Just atoms -> Just $ AnswerSet.buildGameState (AnswerSet.parseAnswerSet atoms) time
                Nothing -> Nothing
            _ -> Nothing
      case maybeGameState of
        Nothing -> pure unit  -- No game state available, skip validation
        Just gameState -> do
          -- Validate: check if toPlayer has a role that's in never_applied_to for this token
          let players = map (\p -> { name: p.name, role: p.role }) gameState.players
          case TC.validateReminderDrop { token, toPlayer } constraints players of
            Just errorMsg -> do
              -- Validation failed - show warning and don't proceed
              H.modify_ \s -> s { scrollNotification = Just errorMsg }
              _ <- H.fork do
                H.liftAff $ Aff.delay (Milliseconds 4000.0)
                H.modify_ \s -> s { scrollNotification = Nothing }
              pure unit
            Nothing -> do
              -- Validation passed - proceed with the drop
              let instContent = fromMaybe "" $ Map.lookup "inst.lp" state.files
              -- Push current state onto undo stack (before making changes)
              let undoEntry = { instLpContent: instContent
                              , description: "Move " <> token <> " from " <> fromPlayer <> " to " <> toPlayer
                              }
              -- Format the time point for ASP
              let timeStr = formatTimePointForASP time
              -- Create the new constraint
              let newConstraint = "assert_reminder_on(" <> token <> ", " <> toPlayer <> ", " <> timeStr <> ")."
              -- Create the old constraint pattern to comment out
              let oldConstraintPattern = "assert_reminder_on(" <> token <> ", " <> fromPlayer <> ", " <> timeStr <> ")."
              -- Modify the content: comment out old constraint if present, add new one
              let modifiedContent = modifyInstLpForReminder instContent oldConstraintPattern newConstraint token fromPlayer toPlayer
              -- Update the virtual filesystem, push undo entry, and clear redo stack
              H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files
                                , undoStack = snoc s.undoStack undoEntry
                                , redoStack = []  -- Clear redo stack on new action
                                }
              -- Re-run Clingo with the new constraints
              handleAction RunClingo
    TG.RoleMoved { role, fromPlayer, toPlayer, time } -> do
      -- Validate the drop using never_applied_to constraints
      state <- H.get
      let constraints = TC.parseNeverAppliedTo state.files
      -- Get current game state to check reminders on target player
      -- In pre-solve mode (no answer sets), we skip validation but still update inst.lp
      let maybeGameState = case state.result of
            Just (ResultSuccess answerSets) ->
              case index answerSets state.selectedModelIndex of
                Just atoms -> Just $ AnswerSet.buildGameState (AnswerSet.parseAnswerSet atoms) time
                Nothing -> Nothing
            _ -> Nothing
      -- Track whether we're in post-solve mode (have a valid answer set)
      let isPostSolve = isJust maybeGameState
      -- Get reminders for validation (empty if no game state - validation always passes)
      let reminders = case maybeGameState of
            Just gameState -> map (\r -> { token: r.token, player: r.player }) gameState.reminders
            Nothing -> []  -- No reminders to validate against in pre-solve mode
      -- Validate: check if toPlayer has any reminder tokens that can't be on this role
      case TC.validateRoleDrop { role, toPlayer } constraints reminders of
        Just errorMsg -> do
          -- Validation failed - show warning and don't proceed
          H.modify_ \s -> s { scrollNotification = Just errorMsg }
          _ <- H.fork do
            H.liftAff $ Aff.delay (Milliseconds 4000.0)
            H.modify_ \s -> s { scrollNotification = Nothing }
          pure unit
        Nothing -> do
          -- Validation passed (or skipped in pre-solve mode) - proceed with the drop
          let instContent = fromMaybe "" $ Map.lookup "inst.lp" state.files
          -- Check if dropping onto the bag (special target)
          if toPlayer == "__bag__" then do
            -- Dropping onto the bag: add assert_drawn or assert_distrib
            -- Roles marked never_in_bag (like drunk) should use assert_distrib instead
            let neverInBagRoles = ["drunk", "marionette"]  -- Roles that can't be physically in the bag
            let isNeverInBag = elem role neverInBagRoles
            let newConstraint = if isNeverInBag
                  then "assert_distrib(" <> role <> ")."
                  else "assert_drawn(" <> role <> ")."
            let undoEntry = { instLpContent: instContent
                            , description: if fromPlayer == "__bluffs__"
                                then "Move " <> role <> " from bluffs to bag"
                                else "Add " <> role <> " to bag"
                            }
            -- Add the constraint to inst.lp, and if from bluffs, remove from bluffs
            let modifiedContent = if fromPlayer == "__bluffs__"
                  then commentOutConstraint instContent ("assert_bluff(" <> role <> ").") <> "\n" <> newConstraint
                  else instContent <> "\n" <> newConstraint
            H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files
                              , undoStack = snoc s.undoStack undoEntry
                              , redoStack = []
                              }
            -- Update syntax highlighting overlay if inst.lp is currently displayed
            state' <- H.get
            when (state'.currentFile == "inst.lp") do
              liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" modifiedContent
            -- Only re-run Clingo in post-solve mode; in pre-solve mode, state update triggers re-render
            if isPostSolve then handleAction RunClingo else pure unit
          else if toPlayer == "__bluffs__" then do
            -- Dropping onto the bluffs panel: add assert_bluff
            -- If from bag, remove from bag (comment out assert_drawn)
            let newConstraint = "assert_bluff(" <> role <> ")."
            let undoEntry = { instLpContent: instContent
                            , description: if fromPlayer == "__bag__"
                                then "Move " <> role <> " from bag to bluffs"
                                else "Add " <> role <> " as bluff"
                            }
            -- Add the constraint to inst.lp, and if from bag, comment out the bag constraint
            let modifiedContent = if fromPlayer == "__bag__"
                  then commentOutConstraint instContent ("assert_drawn(" <> role <> ").") <> "\n" <> newConstraint
                  else instContent <> "\n" <> newConstraint
            H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files
                              , undoStack = snoc s.undoStack undoEntry
                              , redoStack = []
                              }
            -- Update syntax highlighting overlay if inst.lp is currently displayed
            state' <- H.get
            when (state'.currentFile == "inst.lp") do
              liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" modifiedContent
            -- Only re-run Clingo in post-solve mode; in pre-solve mode, state update triggers re-render
            if isPostSolve then handleAction RunClingo else pure unit
          else do
            -- Normal drop onto a player
            -- Detect if this is a copy operation from script/bag (special sources)
            -- Bluffs is NOT a copy operation - dragging from bluffs removes from bluffs
            let isCopyOperation = take 2 fromPlayer == "__" && fromPlayer /= "__bluffs__"
            let sourceDescription = case fromPlayer of
                  "__script__" -> "script"
                  "__bag__" -> "bag"
                  "__bluffs__" -> "bluffs"
                  _ -> fromPlayer
            -- Push current state onto undo stack (before making changes)
            let undoEntry = { instLpContent: instContent
                            , description: if isCopyOperation
                                then "Assign " <> role <> " from " <> sourceDescription <> " to " <> toPlayer
                                else if fromPlayer == "__bluffs__"
                                  then "Move " <> role <> " from bluffs to " <> toPlayer
                                  else "Move " <> role <> " from " <> fromPlayer <> " to " <> toPlayer
                            }
            -- Create the new constraint (forces toPlayer to receive this token)
            let newConstraint = "assert_received(" <> toPlayer <> ", " <> role <> ")."
            -- Create the old constraint pattern to comment out (the previous drag for this token)
            -- For copy operations, we use a dummy pattern that won't match anything
            let oldConstraintPattern = if isCopyOperation
                  then "__NOMATCH__"
                  else "assert_received(" <> fromPlayer <> ", " <> role <> ")."
            -- Modify the content: comment out old constraint if present, add new one
            -- For copy operations, use sourceDescription for cleaner comments
            let effectiveFromPlayer = if isCopyOperation then sourceDescription else fromPlayer
            -- If from bluffs, also remove from bluffs
            let baseContent = if fromPlayer == "__bluffs__"
                  then commentOutConstraint instContent ("assert_bluff(" <> role <> ").")
                  else instContent
            let modifiedContent = modifyInstLpForRole baseContent oldConstraintPattern newConstraint role effectiveFromPlayer toPlayer
            -- Update the virtual filesystem, push undo entry, and clear redo stack
            H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files
                              , undoStack = snoc s.undoStack undoEntry
                              , redoStack = []  -- Clear redo stack on new action
                              }
            -- Update syntax highlighting overlay if inst.lp is currently displayed
            state' <- H.get
            when (state'.currentFile == "inst.lp") do
              liftEffect $ TU.updateHighlightOverlay "editor-textarea" "editor-highlight-overlay" modifiedContent
            -- Only re-run Clingo in post-solve mode; in pre-solve mode, state update triggers re-render
            if isPostSolve then handleAction RunClingo else pure unit

  ClearScrollNotification ->
    H.modify_ \s -> s { scrollNotification = Nothing }

  Undo -> do
    state <- H.get
    case unsnoc state.undoStack of
      Nothing -> pure unit  -- Nothing to undo
      Just { init: remainingUndo, last: undoEntry } -> do
        -- Save current state to redo stack before restoring
        let currentInstLp = fromMaybe "" $ Map.lookup "inst.lp" state.files
        let redoEntry = { instLpContent: currentInstLp
                        , description: undoEntry.description
                        }
        -- Restore the previous inst.lp content
        H.modify_ \s -> s { files = Map.insert "inst.lp" undoEntry.instLpContent s.files
                          , undoStack = remainingUndo
                          , redoStack = snoc s.redoStack redoEntry
                          }
        -- Re-run Clingo to update the grimoire
        handleAction RunClingo

  Redo -> do
    state <- H.get
    case unsnoc state.redoStack of
      Nothing -> pure unit  -- Nothing to redo
      Just { init: remainingRedo, last: redoEntry } -> do
        -- Save current state to undo stack before re-applying
        let currentInstLp = fromMaybe "" $ Map.lookup "inst.lp" state.files
        let undoEntry = { instLpContent: currentInstLp
                        , description: redoEntry.description
                        }
        -- Restore the redo'd inst.lp content
        H.modify_ \s -> s { files = Map.insert "inst.lp" redoEntry.instLpContent s.files
                          , undoStack = snoc s.undoStack undoEntry
                          , redoStack = remainingRedo
                          }
        -- Re-run Clingo to update the grimoire
        handleAction RunClingo

  TextareaClicked -> do
    -- Check if the cursor is on an #include directive
    maybeIncludePath <- liftEffect $ TU.getIncludeAtCursor "editor-textarea"
    case maybeIncludePath of
      Just includePath -> do
        -- Resolve the include path relative to current file
        state <- H.get
        let currentDir = getParentDir state.currentFile
        let resolvedPath = resolveIncludePath currentDir includePath state.files
        H.modify_ \s -> s { navigateIncludeTarget = Just resolvedPath }
      Nothing ->
        pure unit  -- Not on an include, do nothing

  ConfirmNavigateToInclude targetPath -> do
    -- Navigate to the included file
    H.modify_ \s -> s { currentFile = targetPath, navigateIncludeTarget = Nothing }

  CancelNavigateToInclude -> do
    H.modify_ \s -> s { navigateIncludeTarget = Nothing }
    -- Refocus the textarea so cursor position is preserved
    liftEffect $ TU.focusTextarea "editor-textarea"

  ShowTimingDiff entry ->
    H.modify_ \s -> s { selectedTimingEntry = Just entry }

  CloseDiffModal ->
    H.modify_ \s -> s { selectedTimingEntry = Nothing }

  CopyToClipboard text -> do
    _ <- liftEffect $ TU.copyToClipboard text
    pure unit

  CopyToClipboardStopPropagation mouseEvent text -> do
    _ <- liftEffect $ TU.copyToClipboardWithEvent (toEvent mouseEvent) text
    pure unit

  NoOp ->
    pure unit  -- Do nothing, used to stop event propagation

-- Helper for String.take
take :: Int -> String -> String
take = String.take

-- Helper for fromFoldable
fromFoldable :: forall f a. Foldable f => f a -> Array a
fromFoldable = Array.fromFoldable

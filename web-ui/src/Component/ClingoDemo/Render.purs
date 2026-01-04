-- | Render functions for ClingoDemo component
-- | Extracted from ClingoDemo.purs to reduce file size
module Component.ClingoDemo.Render where

import Prelude

import AspParser as ASP
import BuildInfo as BI
import Data.Array (filter, length, mapWithIndex, null, slice, sort)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import FilterExpression as FE
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Component.ClingoDemo.Types (Action(..), State, Slots, ResultDisplay(..), TimingEntry, _timelineGrimoire, answerSetPageSize)
import Component.ClingoDemo.Utils (availableFiles, getDirectories, getParentDir, getFileName, isInDirectory, isRootFile, getVisibleTabs, getCurrentFileContent, getSources, getGrimoireAtoms, getGrimoireInput, getFileDescription)
import Component.TimelineGrimoire as TG
import EarlyParser as Early

-- | Render the grimoire section (always visible, using early or answer set atoms)
renderGrimoireSection :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderGrimoireSection state =
  let
    grimoireData = getGrimoireAtoms state
    hasAtoms = not (null grimoireData.atoms)
    -- Extract player_count from files for pre-solve display
    playerCount = Early.extractPlayerCount state.files
    playerCountInfo = case playerCount of
      Just n -> " (player_count = " <> show n <> ")"
      Nothing -> ""
    title = if grimoireData.isEarly
      then "Grimoire (Preview)" <> playerCountInfo
      else "Timeline & Grimoire View" <> grimoireData.modelInfo
    subtitle = if grimoireData.isEarly
      then "Showing player setup from files. Run Clingo to see full game state."
      else "Click on timeline events to highlight the corresponding atom in the answer set below."
  in
  HH.div
    [ HP.style "margin-bottom: 20px;" ]
    [ HH.h2
        [ HP.style "color: #333; margin-bottom: 10px;" ]
        [ HH.text title ]
    , HH.p
        [ HP.style $ "font-size: 12px; margin-bottom: 10px; font-style: italic; "
            <> if grimoireData.isEarly then "color: #FF9800;" else "color: #666;"
        ]
        [ HH.text subtitle ]
    -- Undo/Redo buttons (only show when we have answer sets)
    , if not grimoireData.isEarly
        then HH.div
          [ HP.style "display: flex; gap: 10px; margin-bottom: 10px;" ]
          [ HH.button
              [ HP.style $ "padding: 8px 16px; font-size: 14px; cursor: pointer; "
                  <> "background: " <> (if null state.undoStack then "#ccc" else "#FF9800") <> "; "
                  <> "color: white; border: none; border-radius: 4px; "
                  <> "display: flex; align-items: center; gap: 6px;"
              , HE.onClick \_ -> Undo
              , HP.disabled (null state.undoStack)
              , HP.title $ if null state.undoStack then "Nothing to undo" else "Undo last token drag"
              ]
              [ HH.text "↩ Undo"
              , if not (null state.undoStack)
                then HH.span
                  [ HP.style "font-size: 11px; opacity: 0.8;" ]
                  [ HH.text $ "(" <> show (length state.undoStack) <> ")" ]
                else HH.text ""
              ]
          , HH.button
              [ HP.style $ "padding: 8px 16px; font-size: 14px; cursor: pointer; "
                  <> "background: " <> (if null state.redoStack then "#ccc" else "#2196F3") <> "; "
                  <> "color: white; border: none; border-radius: 4px; "
                  <> "display: flex; align-items: center; gap: 6px;"
              , HE.onClick \_ -> Redo
              , HP.disabled (null state.redoStack)
              , HP.title $ if null state.redoStack then "Nothing to redo" else "Redo last undone action"
              ]
              [ HH.text "↪ Redo"
              , if not (null state.redoStack)
                then HH.span
                  [ HP.style "font-size: 11px; opacity: 0.8;" ]
                  [ HH.text $ "(" <> show (length state.redoStack) <> ")" ]
                else HH.text ""
              ]
          ]
        else HH.text ""
    , if hasAtoms
        then HH.slot _timelineGrimoire unit TG.component (getGrimoireInput state) HandleTimelineEvent
        else HH.div
          [ HP.style "padding: 20px; background: #f5f5f5; border-radius: 4px; color: #666;" ]
          [ HH.text "No player data found. Add chair/2 facts to players.lp to see the grimoire." ]
    ]

-- | Render the component
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  let
    sources = getSources state
    parsed = ASP.parseProgram sources
    currentContent = getCurrentFileContent state
  in
  HH.div
    [ HP.style "font-family: system-ui, sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px; position: relative;" ]
    [ HH.h1_ [ HH.text "Clingo WASM + PureScript Demo" ]
    , HH.p
        [ HP.style "color: #666;" ]
        [ HH.text "Blood on the Clocktower ASP Explorer" ]
    , case BI.latestMergedPR of
        Just prNum ->
          HH.p
            [ HP.style "color: #999; font-size: 12px; margin-top: -10px;" ]
            [ HH.text "Latest merged: "
            , HH.a
                [ HP.href $ "https://github.com/pnkfelix/botc-asp/pull/" <> show prNum
                , HP.target "_blank"
                , HP.style "color: #4CAF50; text-decoration: none;"
                ]
                [ HH.text $ "PR #" <> show prNum ]
            ]
        Nothing ->
          HH.text ""

    -- Floating action buttons (bottom right)
    , HH.div
        [ HP.style "position: fixed; bottom: 20px; right: 20px; z-index: 100; display: flex; flex-direction: column; gap: 10px;" ]
        [ -- File directory button
          HH.button
            [ HP.style $ "padding: 12px 16px; font-size: 14px; cursor: pointer; "
                <> "background: #FF9800; color: white; border: none; border-radius: 50px; "
                <> "box-shadow: 0 2px 8px rgba(0,0,0,0.3);"
            , HE.onClick \_ -> ToggleFileDirectory
            ]
            [ HH.text "Files" ]
        , -- Predicate navigator toggle button
          HH.button
            [ HP.style $ "padding: 12px 16px; font-size: 14px; cursor: pointer; "
                <> "background: #2196F3; color: white; border: none; border-radius: 50px; "
                <> "box-shadow: 0 2px 8px rgba(0,0,0,0.3);"
            , HE.onClick \_ -> TogglePredicateList
            ]
            [ HH.text $ if state.showPredicateList then "Hide Predicates" else "Predicates" ]
        ]

    -- Scroll notification toast (top center, appears briefly when scrolling fails)
    , case state.scrollNotification of
        Just msg ->
          HH.div
            [ HP.style $ "position: fixed; top: 20px; left: 50%; transform: translateX(-50%); "
                <> "z-index: 1000; padding: 12px 24px; background: #f44336; color: white; "
                <> "border-radius: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.3); "
                <> "font-size: 14px; max-width: 80%; text-align: center;"
            , HE.onClick \_ -> ClearScrollNotification
            ]
            [ HH.text msg ]
        Nothing ->
          HH.text ""

    -- Single file editor with file tabs
    , HH.div
        [ HP.style "margin: 20px 0;" ]
        [ -- File tabs (show root files + current file if in a subdirectory)
          HH.div
            [ HP.style "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 8px;" ]
            (map (renderFileTab state.currentFile) (getVisibleTabs state.currentFile))
        , -- Editor container with syntax highlighting overlay
          HH.div
            [ HP.style "position: relative;" ]
            [ -- Syntax highlight overlay (behind textarea)
              HH.pre
                [ HP.id "editor-highlight-overlay"
                , HP.style $ "position: absolute; top: 0; left: 0; right: 0; bottom: 0; "
                    <> "margin: 0; padding: 10px; border: 1px solid transparent; "
                    <> "font-family: monospace; font-size: 12px; line-height: inherit; "
                    <> "overflow: hidden; pointer-events: none; white-space: pre-wrap; "
                    <> "word-wrap: break-word; color: #333; background: white; "
                    <> "border-radius: 4px;"
                ]
                [ HH.text "" ]  -- Content set via JS
            , -- Editor textarea (transparent text, on top)
              HH.textarea
                [ HP.style $ "width: 100%; height: 400px; font-family: monospace; font-size: 12px; "
                    <> "padding: 10px; border: 1px solid #ccc; border-radius: 4px; "
                    <> "resize: vertical; overflow: auto; position: relative; "
                    <> "background: transparent; color: transparent; caret-color: black; "
                    <> "-webkit-text-fill-color: transparent;"
                , HP.id "editor-textarea"
                , HP.value currentContent
                , HE.onValueInput SetFileContent
                , HE.onClick \_ -> TextareaClicked
                , HP.disabled state.isLoading
                ]
            ]
        ]

    -- Controls: model limit input, run button, cancel button
    , HH.div
        [ HP.style "margin: 20px 0; text-align: center; display: flex; flex-wrap: wrap; justify-content: center; align-items: center; gap: 15px;" ]
        [ -- Model limit input
          HH.label
            [ HP.style "display: flex; align-items: center; gap: 8px;" ]
            [ HH.text "Max models:"
            , HH.input
                [ HP.style "width: 80px; padding: 8px; font-size: 14px; border: 1px solid #ccc; border-radius: 4px;"
                , HP.type_ HP.InputNumber
                , HP.placeholder "5 (0=all)"
                , HP.value state.modelLimit
                , HE.onValueInput SetModelLimit
                , HP.disabled state.isLoading
                ]
            ]
        , -- Run button
          HH.button
            [ HP.style $ "padding: 12px 30px; font-size: 18px; cursor: pointer; "
                <> "background: #4CAF50; color: white; border: none; border-radius: 4px;"
                <> if state.isLoading then " opacity: 0.6;" else ""
            , HE.onClick \_ -> RunClingo
            , HP.disabled (state.isLoading || not state.isInitialized)
            ]
            [ HH.text $ if state.isLoading
                then "Running..."
                else if state.isInitialized
                  then "Run Clingo"
                  else "Initializing..."
            ]
        , -- Cancel button (only shown when loading)
          if state.isLoading
            then HH.button
              [ HP.style "padding: 12px 30px; font-size: 18px; cursor: pointer; background: #f44336; color: white; border: none; border-radius: 4px;"
              , HE.onClick \_ -> CancelSolve
              ]
              [ HH.text "Cancel" ]
            else HH.text ""
        ]

    -- Grimoire section (always visible - uses early parsing or answer set)
    , renderGrimoireSection state

    -- Results display (status and answer set output)
    , renderResult state

    -- Timing history table (shows after any runs)
    , renderTimingTable state.timingHistory

    -- File directory popup with tree view
    , renderFileDirectory state.showFileDirectory state.currentFile state.expandedDirs

    -- Predicate list panel (slide-in from right)
    , renderPredicatePanel state.showPredicateList parsed.predicates

    -- Modal for predicate references
    , renderPredicateModal state.selectedPredicate sources parsed.references

    -- Dialog for navigating to included files
    , renderNavigateIncludeDialog state.navigateIncludeTarget state.files

    -- Diff modal for timing table entries
    , renderDiffModal state.selectedTimingEntry
    ]

-- | Render a file tab button
renderFileTab :: forall m. String -> String -> H.ComponentHTML Action Slots m
renderFileTab currentFile filePath =
  let
    isSelected = filePath == currentFile
    displayName = getFileName filePath
    -- Add directory prefix if file is not at root
    prefix = if isRootFile filePath then "" else getParentDir filePath <> "/"
    baseStyle = "padding: 8px 16px; font-size: 14px; cursor: pointer; border: none; border-radius: 4px 4px 0 0; "
    selectedStyle = if isSelected
      then "background: #4CAF50; color: white; font-weight: bold;"
      else "background: #e0e0e0; color: #333;"
  in
  HH.button
    [ HP.style $ baseStyle <> selectedStyle
    , HE.onClick \_ -> SelectFile filePath
    , HP.title filePath  -- Tooltip shows full path
    ]
    [ if prefix /= ""
      then HH.span
        [ HP.style "font-size: 10px; opacity: 0.7;" ]
        [ HH.text prefix ]
      else HH.text ""
    , HH.text displayName
    ]

-- | Render the file directory popup with tree view
renderFileDirectory :: forall m. Boolean -> String -> Set.Set String -> H.ComponentHTML Action Slots m
renderFileDirectory isVisible currentFile expandedDirs =
  let
    -- Get all unique directories sorted
    allDirs = sort $ getDirectories availableFiles
    -- Get root-level files
    rootFiles = sort $ filter isRootFile availableFiles
  in
  HH.div_
    [ -- Backdrop (click to close)
      HH.div
        [ HP.style $ "position: fixed; top: 0; left: 0; right: 0; bottom: 0; "
            <> "background: rgba(0,0,0,0.3); z-index: 98; "
            <> "opacity: " <> (if isVisible then "1" else "0") <> "; "
            <> "pointer-events: " <> (if isVisible then "auto" else "none") <> "; "
            <> "transition: opacity 0.3s ease;"
        , HE.onClick \_ -> ToggleFileDirectory
        ]
        []
    -- Panel
    , HH.div
        [ HP.style $ "position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); "
            <> "background: white; border-radius: 8px; padding: 20px; "
            <> "box-shadow: 0 4px 20px rgba(0,0,0,0.3); z-index: 99; "
            <> "min-width: 350px; max-width: 500px; max-height: 80vh; "
            <> "opacity: " <> (if isVisible then "1" else "0") <> "; "
            <> "pointer-events: " <> (if isVisible then "auto" else "none") <> "; "
            <> "transition: opacity 0.3s ease; "
            <> "display: flex; flex-direction: column;"
        , HE.onClick \_ -> NoOp  -- Prevent clicks from closing via backdrop
        ]
        [ -- Header
          HH.div
            [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; flex-shrink: 0;" ]
            [ HH.h3
                [ HP.style "margin: 0; color: #333;" ]
                [ HH.text $ "Files (" <> show (length availableFiles) <> ")" ]
            , HH.button
                [ HP.style "background: transparent; border: none; font-size: 20px; cursor: pointer; padding: 0 5px;"
                , HE.onClick \_ -> ToggleFileDirectory
                ]
                [ HH.text "×" ]
            ]
        -- Tree view (scrollable)
        , HH.div
            [ HP.style "overflow-y: auto; flex: 1;" ]
            [ HH.div
                [ HP.style "display: flex; flex-direction: column; gap: 2px;" ]
                ( -- Root files first
                  map (renderFileItem currentFile 0) rootFiles
                  -- Then directories with their contents
                  <> (allDirs >>= \dir -> renderDirectoryNode currentFile expandedDirs dir 0)
                )
            ]
        ]
    ]

-- | Render a directory node with its contents
renderDirectoryNode :: forall m. String -> Set.Set String -> String -> Int -> Array (H.ComponentHTML Action Slots m)
renderDirectoryNode currentFile expandedDirs dir depth =
  let
    isExpanded = Set.member dir expandedDirs
    dirFiles = sort $ filter (isInDirectory dir) availableFiles
    indent = depth * 16
    -- Check if current file is in this directory (to highlight directory)
    hasSelectedFile = getParentDir currentFile == dir
  in
  [ -- Directory header (clickable to expand/collapse)
    HH.div
      [ HP.style $ "display: flex; align-items: center; padding: 8px 12px; "
          <> "margin-left: " <> show indent <> "px; "
          <> "background: " <> (if hasSelectedFile then "#fff3e0" else "#f0f0f0") <> "; "
          <> "border: 1px solid " <> (if hasSelectedFile then "#FF9800" else "#ddd") <> "; "
          <> "border-radius: 4px; cursor: pointer; "
          <> "font-weight: bold; color: #555;"
      , HE.onClick \_ -> ToggleDirectory dir
      ]
      [ -- Expand/collapse icon
        HH.span
          [ HP.style "margin-right: 8px; font-family: monospace; width: 12px;" ]
          [ HH.text $ if isExpanded then "▼" else "▶" ]
      , -- Folder icon
        HH.span
          [ HP.style "margin-right: 8px;" ]
          [ HH.text $ if isExpanded then "📂" else "📁" ]
      , -- Directory name
        HH.span_
          [ HH.text $ getFileName dir <> "/" ]
      , -- File count badge
        HH.span
          [ HP.style "margin-left: auto; font-size: 11px; color: #888; font-weight: normal;" ]
          [ HH.text $ "(" <> show (length dirFiles) <> ")" ]
      ]
  ] <>
  -- Contents (only if expanded)
  if isExpanded
    then map (renderFileItem currentFile (depth + 1)) dirFiles
    else []

-- | Render a file item in the directory tree
renderFileItem :: forall m. String -> Int -> String -> H.ComponentHTML Action Slots m
renderFileItem currentFile depth filePath =
  let
    isSelected = filePath == currentFile
    displayName = getFileName filePath
    indent = depth * 16
    description = getFileDescription filePath
  in
  HH.button
    [ HP.style $ "display: flex; align-items: center; width: 100%; text-align: left; "
        <> "padding: 8px 12px; margin-left: " <> show indent <> "px; "
        <> "background: " <> (if isSelected then "#e8f5e9" else "white") <> "; "
        <> "border: " <> (if isSelected then "2px solid #4CAF50" else "1px solid #eee") <> "; "
        <> "border-radius: 4px; cursor: pointer;"
    , HE.onClick \_ -> SelectFile filePath
    , HP.title filePath  -- Full path as tooltip
    ]
    [ -- File icon
      HH.span
        [ HP.style "margin-right: 8px; opacity: 0.6;" ]
        [ HH.text "📄" ]
    , -- File info
      HH.div
        [ HP.style "flex: 1; min-width: 0;" ]
        [ HH.div
            [ HP.style $ "font-weight: " <> (if isSelected then "bold" else "normal") <> "; "
                <> "font-family: monospace; font-size: 13px; "
                <> "white-space: nowrap; overflow: hidden; text-overflow: ellipsis;"
            ]
            [ HH.text displayName ]
        , if description /= ""
            then HH.div
              [ HP.style "font-size: 11px; color: #888; margin-top: 2px;" ]
              [ HH.text description ]
            else HH.text ""
        ]
    ]

-- | Render the result section
renderResult :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderResult state = case state.result of
  Nothing ->
    HH.div
      [ HP.style "padding: 20px; background: #f5f5f5; border-radius: 4px; color: #666;" ]
      [ HH.text "Click 'Run Clingo' to solve the program" ]

  Just (ResultError err) ->
    HH.div
      [ HP.style "padding: 20px; background: #ffebee; border-radius: 4px; color: #c62828; font-family: monospace; white-space: pre-wrap; position: relative;" ]
      [ HH.div
          [ HP.style "display: flex; justify-content: space-between; align-items: flex-start;" ]
          [ HH.div_
              [ HH.strong_ [ HH.text "Error: " ]
              , HH.text err
              ]
          , renderCopyButton err
          ]
      ]

  Just ResultUnsat ->
    HH.div
      [ HP.style "padding: 20px; background: #fff3e0; border-radius: 4px; color: #e65100;" ]
      [ HH.strong_ [ HH.text "UNSATISFIABLE" ]
      , HH.p_ [ HH.text "No solutions exist for the given constraints." ]
      ]

  Just (ResultSuccess answerSets) ->
    let
      selectedIdx = state.selectedModelIndex
      -- Pagination: only render a subset of answer sets to prevent browser crash
      totalCount = length answerSets
      pageStart = state.answerSetPage * answerSetPageSize
      pageEnd = min (pageStart + answerSetPageSize) totalCount
      pageItems = slice pageStart pageEnd answerSets
      totalPages = (totalCount + answerSetPageSize - 1) / answerSetPageSize
      currentPage = state.answerSetPage + 1
      hasPrevPage = state.answerSetPage > 0
      hasNextPage = pageEnd < totalCount
    in
    -- Output section (grimoire is now rendered separately in renderGrimoireSection)
    HH.div
          [ HP.style "padding: 15px; background: #e8f5e9; border-radius: 4px;" ]
          [ HH.strong
              [ HP.style "color: #2e7d32;" ]
              [ HH.text $ "SATISFIABLE - Found " <> show totalCount <> " answer set(s)"
                  <> if totalCount > 1 then " (click to select)" else ""
              ]
          -- Pagination controls (only show if more than one page)
          , if totalPages > 1
            then HH.div
              [ HP.style "display: flex; align-items: center; gap: 10px; margin-top: 10px;" ]
              [ HH.button
                  [ HP.style $ "padding: 6px 12px; font-size: 14px; cursor: pointer; "
                      <> "background: " <> (if hasPrevPage then "#4CAF50" else "#ccc") <> "; "
                      <> "color: white; border: none; border-radius: 4px;"
                  , HE.onClick \_ -> PrevAnswerSetPage
                  , HP.disabled (not hasPrevPage)
                  ]
                  [ HH.text "← Prev" ]
              , HH.span
                  [ HP.style "font-size: 14px; color: #666;" ]
                  [ HH.text $ "Page " <> show currentPage <> " of " <> show totalPages
                      <> " (showing " <> show (pageStart + 1) <> "-" <> show pageEnd <> ")"
                  ]
              , HH.button
                  [ HP.style $ "padding: 6px 12px; font-size: 14px; cursor: pointer; "
                      <> "background: " <> (if hasNextPage then "#4CAF50" else "#ccc") <> "; "
                      <> "color: white; border: none; border-radius: 4px;"
                  , HE.onClick \_ -> NextAnswerSetPage
                  , HP.disabled (not hasNextPage)
                  ]
                  [ HH.text "Next →" ]
              ]
            else HH.text ""
          -- Filter input
          , HH.div
              [ HP.style "margin-top: 10px; display: flex; align-items: center; gap: 10px; flex-wrap: wrap;" ]
              [ HH.label
                  [ HP.style "display: flex; align-items: center; gap: 6px; flex: 1; min-width: 200px;" ]
                  [ HH.span
                      [ HP.style "font-size: 12px; color: #666; white-space: nowrap;" ]
                      [ HH.text "Filter:" ]
                  , HH.input
                      [ HP.style $ "flex: 1; padding: 6px 8px; font-size: 12px; font-family: monospace; "
                          <> "border: 1px solid #ccc; border-radius: 4px; min-width: 150px;"
                      , HP.type_ HP.InputText
                      , HP.placeholder "e.g., assigned or tells, not time, alice and chef"
                      , HP.value state.outputFilter
                      , HE.onValueInput SetOutputFilter
                      ]
                  ]
              , HH.span
                  [ HP.style "font-size: 11px; color: #888; font-style: italic;" ]
                  [ HH.text $ "Using: \"" <> state.outputFilter <> "\"" ]
              ]
          , HH.div
              [ HP.id "answer-set-display"
              , HP.style $ "max-height: 200px; overflow-y: scroll; margin-top: 10px; "
                  <> "-webkit-overflow-scrolling: touch; overscroll-behavior: contain; "
                  <> "touch-action: pan-y;"
              ]
              -- Render only the current page of answer sets (with correct global indices)
              [ HH.div_ $ mapWithIndex (\pageIdx atoms -> renderAnswerSet state.outputFilter selectedIdx (pageStart + pageIdx) atoms) pageItems ]
          ]
  where
    renderAnswerSet filterExpr selectedIdx idx atoms =
      let
        isSelected = idx == selectedIdx
        -- Apply filter to atoms (handles space-separated atoms in array elements)
        filteredAtoms = FE.filterAtoms filterExpr atoms
        -- Count individual atoms (not lines) for accurate display
        totalAtomCount = FE.countAtoms atoms
        filteredAtomCount = length filteredAtoms
        isFiltered = filterExpr /= "" && filteredAtomCount /= totalAtomCount
        atomsText = intercalate " " filteredAtoms
        baseStyle = "margin-top: 8px; padding: 8px; border-radius: 4px; cursor: pointer; transition: all 0.2s; "
        selectedStyle = if isSelected
          then "background: #c8e6c9; border: 2px solid #4CAF50;"
          else "background: white; border: 1px solid #c8e6c9;"
        hoverNote = if not isSelected then " opacity: 0.9;" else ""
      in
      HH.div
        [ HP.style $ baseStyle <> selectedStyle <> hoverNote <> " touch-action: pan-y;"
        , HE.onClick \_ -> SelectModel idx
        ]
        [ HH.div
            [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;" ]
            [ HH.span
                [ HP.style $ "font-weight: bold; font-size: 12px; "
                    <> if isSelected then "color: #1b5e20;" else "color: #388e3c;"
                ]
                [ HH.text $ "Answer Set " <> show (idx + 1)
                    <> (if isSelected then " ✓" else "")
                    <> (if isFiltered then " (" <> show filteredAtomCount <> "/" <> show totalAtomCount <> " atoms)" else "")
                ]
            , renderCopyButtonSmall atomsText
            ]
        , HH.code
            [ HP.style $ "display: block; font-family: monospace; white-space: pre-wrap; color: #1b5e20; font-size: 11px; max-height: 60px; overflow-y: auto; "
                <> "-webkit-overflow-scrolling: touch;"
            ]
            [ HH.text atomsText ]
        ]

-- | Render a copy button with clipboard icon
-- | Uses inline SVG for the icon
renderCopyButton :: forall m. String -> H.ComponentHTML Action Slots m
renderCopyButton textToCopy =
  HH.button
    [ HP.style $ "background: transparent; border: 1px solid #ccc; border-radius: 4px; "
        <> "padding: 4px 8px; cursor: pointer; display: flex; align-items: center; gap: 4px; "
        <> "color: #666; font-size: 11px; transition: all 0.2s; flex-shrink: 0;"
    , HP.title "Copy to clipboard"
    , HE.onClick \_ -> CopyToClipboard textToCopy
    ]
    [ renderCopyIcon
    , HH.text "Copy"
    ]

-- | Render a smaller copy button (icon only) for compact spaces
-- | Stops event propagation to prevent triggering parent click handlers
renderCopyButtonSmall :: forall m. String -> H.ComponentHTML Action Slots m
renderCopyButtonSmall textToCopy =
  HH.button
    [ HP.style $ "background: transparent; border: 1px solid #ccc; border-radius: 3px; "
        <> "padding: 2px 4px; cursor: pointer; display: flex; align-items: center; "
        <> "color: #666; transition: all 0.2s; flex-shrink: 0;"
    , HP.title "Copy to clipboard"
    , HE.onClick \e -> CopyToClipboardStopPropagation e textToCopy
    ]
    [ renderCopyIcon
    ]

-- | Render the clipboard/copy icon SVG
renderCopyIcon :: forall m w i. HH.HTML w i
renderCopyIcon =
  HH.span
    [ HP.style "width: 14px; height: 14px; display: inline-block;" ]
    [ HH.element (HH.ElemName "svg")
        [ HP.attr (HH.AttrName "viewBox") "0 0 24 24"
        , HP.attr (HH.AttrName "fill") "none"
        , HP.attr (HH.AttrName "stroke") "currentColor"
        , HP.attr (HH.AttrName "stroke-width") "2"
        , HP.style "width: 100%; height: 100%;"
        ]
        [ HH.element (HH.ElemName "rect")
            [ HP.attr (HH.AttrName "x") "9"
            , HP.attr (HH.AttrName "y") "9"
            , HP.attr (HH.AttrName "width") "13"
            , HP.attr (HH.AttrName "height") "13"
            , HP.attr (HH.AttrName "rx") "2"
            , HP.attr (HH.AttrName "ry") "2"
            ]
            []
        , HH.element (HH.ElemName "path")
            [ HP.attr (HH.AttrName "d") "M5 15H4a2 2 0 0 1-2-2V4a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"
            ]
            []
        ]
    ]

-- | Render the timing history table
-- | Shows a table with diff description, model limit, and timing for each run
renderTimingTable :: forall m. Array TimingEntry -> H.ComponentHTML Action Slots m
renderTimingTable entries =
  if null entries
    then HH.text ""  -- Don't render anything if no timing data
    else
      HH.div
        [ HP.style "margin: 20px 0; padding: 15px; background: #f9f9f9; border-radius: 4px; border: 1px solid #e0e0e0;" ]
        [ HH.h3
            [ HP.style "margin: 0 0 10px 0; color: #333; font-size: 14px;" ]
            [ HH.text $ "Clingo Timing History (" <> show (length entries) <> " runs)" ]
        , HH.p
            [ HP.style "margin: 0 0 10px 0; color: #666; font-size: 11px; font-style: italic;" ]
            [ HH.text "Click a row to see the detailed diff (comments stripped)" ]
        , HH.div
            [ HP.style "overflow-x: auto;" ]
            [ HH.table
                [ HP.style "width: 100%; border-collapse: collapse; font-size: 12px; font-family: monospace;" ]
                [ HH.thead_
                    [ HH.tr
                        [ HP.style "background: #e8e8e8;" ]
                        [ HH.th [ HP.style "padding: 8px; text-align: left; border-bottom: 2px solid #ccc;" ] [ HH.text "#" ]
                        , HH.th [ HP.style "padding: 8px; text-align: left; border-bottom: 2px solid #ccc;" ] [ HH.text "Changes vs Original" ]
                        , HH.th [ HP.style "padding: 8px; text-align: right; border-bottom: 2px solid #ccc;" ] [ HH.text "Models (found/max)" ]
                        , HH.th [ HP.style "padding: 8px; text-align: right; border-bottom: 2px solid #ccc;" ] [ HH.text "Solve (s)" ]
                        , HH.th [ HP.style "padding: 8px; text-align: right; border-bottom: 2px solid #ccc;" ] [ HH.text "Total (s)" ]
                        ]
                    ]
                , HH.tbody_ $ map renderTimingRow entries
                ]
            ]
        ]
  where
    renderTimingRow entry =
      HH.tr
        [ HP.style $ "border-bottom: 1px solid #e0e0e0; cursor: pointer; transition: background 0.2s;"
        , HE.onClick \_ -> ShowTimingDiff entry
        ]
        [ HH.td
            [ HP.style "padding: 6px 8px; color: #666;" ]
            [ HH.text $ show entry.runIndex ]
        , HH.td
            [ HP.style "padding: 6px 8px; max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; color: #2196F3; text-decoration: underline;"
            , HP.title $ entry.diffSummary <> " (click for details)"
            ]
            [ HH.text entry.diffSummary ]
        , HH.td
            [ HP.style "padding: 6px 8px; text-align: right;" ]
            [ HH.text $ formatModelCount entry ]
        , HH.td
            [ HP.style "padding: 6px 8px; text-align: right; color: #2196F3;" ]
            [ HH.text $ formatTime entry.solveTime ]
        , HH.td
            [ HP.style "padding: 6px 8px; text-align: right; color: #4CAF50;" ]
            [ HH.text $ formatTime entry.totalTime ]
        ]

    -- Format model count as "actual/max" with "+" if more models exist
    formatModelCount :: TimingEntry -> String
    formatModelCount e =
      let actual = show e.actualModelCount <> (if e.moreModels then "+" else "")
          maxStr = if e.modelLimit == 0 then "all" else show e.modelLimit
      in actual <> "/" <> maxStr

    -- Format time in seconds with appropriate precision
    formatTime :: Number -> String
    formatTime t =
      if t < 0.001
        then "<0.001"
        else if t < 0.01
          then show (floorTo3 (t * 1000.0)) <> "ms"
          else if t < 1.0
            then show (floorTo3 t)
            else show (floorTo2 t)
      where
        floorTo3 n = Int.toNumber (Int.floor (n * 1000.0)) / 1000.0
        floorTo2 n = Int.toNumber (Int.floor (n * 100.0)) / 100.0

-- | Render the diff modal showing detailed file changes
renderDiffModal :: forall m. Maybe TimingEntry -> H.ComponentHTML Action Slots m
renderDiffModal Nothing = HH.text ""
renderDiffModal (Just entry) =
  HH.div
    [ HP.style $ "position: fixed; top: 0; left: 0; right: 0; bottom: 0; "
        <> "background: rgba(0,0,0,0.5); z-index: 200; "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "padding: 20px;"
    , HE.onClick \_ -> CloseDiffModal
    ]
    [ HH.div
        [ HP.style $ "background: white; border-radius: 8px; "
            <> "max-width: 900px; width: 100%; max-height: 80vh; "
            <> "display: flex; flex-direction: column; "
            <> "box-shadow: 0 4px 20px rgba(0,0,0,0.3);"
        , HE.onClick \_ -> NoOp  -- Prevent clicks inside modal from closing it
        ]
        [ -- Modal header
          HH.div
            [ HP.style "padding: 15px 20px; background: #2196F3; color: white; border-radius: 8px 8px 0 0; display: flex; justify-content: space-between; align-items: center;" ]
            [ HH.div_
                [ HH.strong_ [ HH.text $ "Run #" <> show entry.runIndex <> " - Diff Details" ]
                , HH.div
                    [ HP.style "font-size: 12px; opacity: 0.9; margin-top: 4px;" ]
                    [ HH.text $ entry.diffSummary <> " | Models: " <> formatModalModelCount entry
                        <> " | Time: " <> show entry.totalTime <> "s"
                    ]
                ]
            , HH.button
                [ HP.style "background: transparent; border: none; color: white; font-size: 24px; cursor: pointer; padding: 0 5px;"
                , HE.onClick \_ -> CloseDiffModal
                ]
                [ HH.text "×" ]
            ]
        -- Modal body with diffs
        , HH.div
            [ HP.style "flex: 1; overflow-y: auto; padding: 15px;" ]
            [ if null entry.fileDiffs
              then HH.p
                [ HP.style "color: #666; font-style: italic; text-align: center; padding: 20px;" ]
                [ HH.text "No differences from original files (comments excluded)" ]
              else HH.div_ $ map renderFileDiff entry.fileDiffs
            ]
        -- Modal footer
        , HH.div
            [ HP.style "padding: 10px 20px; background: #f5f5f5; border-radius: 0 0 8px 8px; text-align: right; font-size: 12px; color: #666;" ]
            [ HH.text $ show (length entry.fileDiffs) <> " file(s) with changes (comments stripped)" ]
        ]
    ]
  where
    -- Format model count for modal display: "actual/max" with "+" if more exist
    formatModalModelCount :: TimingEntry -> String
    formatModalModelCount e =
      let actual = show e.actualModelCount <> (if e.moreModels then "+" else "")
          maxStr = if e.modelLimit == 0 then "all" else show e.modelLimit
      in actual <> "/" <> maxStr

    renderFileDiff diff =
      let
        -- Build content-addressed sets: all unique line contents from each side
        originalSet = Set.fromFoldable diff.originalLines
        currentSet = Set.fromFoldable diff.currentLines
        -- A line is "unique" to its side if it doesn't appear anywhere on the other side
        isUniqueToOriginal line = not (Set.member line currentSet)
        isUniqueToCurrent line = not (Set.member line originalSet)
        -- Render a single line with optional highlighting
        renderLine isUnique line =
          HH.div
            [ HP.style $ if isUnique line
                then "background: #ffcdd2; padding: 0 4px; margin: 0 -4px;"  -- Red highlight for removed
                else ""
            ]
            [ HH.text line ]
        renderLineGreen isUnique line =
          HH.div
            [ HP.style $ if isUnique line
                then "background: #c8e6c9; padding: 0 4px; margin: 0 -4px;"  -- Green highlight for added
                else ""
            ]
            [ HH.text line ]
      in
      HH.div
        [ HP.style "margin-bottom: 20px; border: 1px solid #ddd; border-radius: 4px; overflow: hidden;" ]
        [ -- File header
          HH.div
            [ HP.style "padding: 8px 12px; background: #f0f0f0; font-weight: bold; font-family: monospace; font-size: 13px; border-bottom: 1px solid #ddd;" ]
            [ HH.text diff.fileName ]
        -- Two-column diff view
        , HH.div
            [ HP.style "display: flex; font-family: monospace; font-size: 11px;" ]
            [ -- Original column
              HH.div
                [ HP.style "flex: 1; border-right: 1px solid #ddd;" ]
                [ HH.div
                    [ HP.style "padding: 4px 8px; background: #ffebee; font-weight: bold; font-size: 10px; color: #c62828;" ]
                    [ HH.text "Original" ]
                , HH.div
                    [ HP.style "padding: 8px; max-height: 300px; overflow-y: auto; background: #fff5f5;" ]
                    (map (renderLine isUniqueToOriginal) diff.originalLines)
                ]
            -- Current column
            , HH.div
                [ HP.style "flex: 1;" ]
                [ HH.div
                    [ HP.style "padding: 4px 8px; background: #e8f5e9; font-weight: bold; font-size: 10px; color: #2e7d32;" ]
                    [ HH.text "Current" ]
                , HH.div
                    [ HP.style "padding: 8px; max-height: 300px; overflow-y: auto; background: #f5fff5;" ]
                    (map (renderLineGreen isUniqueToCurrent) diff.currentLines)
                ]
            ]
        ]

-- | Render the predicate list panel (slide-in from right) with backdrop
renderPredicatePanel :: forall m. Boolean -> Array ASP.Predicate -> H.ComponentHTML Action Slots m
renderPredicatePanel isVisible predicates =
  HH.div_
    [ -- Backdrop (click to close)
      HH.div
        [ HP.style $ "position: fixed; top: 0; left: 0; right: 0; bottom: 0; "
            <> "background: rgba(0,0,0,0.3); z-index: 98; "
            <> "opacity: " <> (if isVisible then "1" else "0") <> "; "
            <> "pointer-events: " <> (if isVisible then "auto" else "none") <> "; "
            <> "transition: opacity 0.3s ease;"
        , HE.onClick \_ -> TogglePredicateList
        ]
        []
    -- Panel
    , HH.div
        [ HP.style $ "position: fixed; top: 0; right: 0; height: 100vh; width: 280px; "
            <> "background: white; box-shadow: -2px 0 8px rgba(0,0,0,0.2); "
            <> "transform: translateX(" <> (if isVisible then "0" else "100%") <> "); "
            <> "transition: transform 0.3s ease; z-index: 99; "
            <> "display: flex; flex-direction: column;"
        , HE.onClick \_ -> NoOp  -- Prevent clicks from closing via backdrop
        ]
        [ -- Header
          HH.div
            [ HP.style "padding: 15px; background: #2196F3; color: white; font-weight: bold; display: flex; justify-content: space-between; align-items: center;" ]
            [ HH.text $ "Predicates (" <> show (length predicates) <> ")"
            , HH.button
                [ HP.style "background: transparent; border: none; color: white; font-size: 20px; cursor: pointer; padding: 0 5px;"
                , HE.onClick \_ -> TogglePredicateList
                ]
                [ HH.text "×" ]
            ]
        -- Scrollable list
        , HH.div
            [ HP.style "flex: 1; overflow-y: auto; padding: 10px;" ]
            [ if null predicates
              then HH.p
                [ HP.style "color: #666; font-style: italic;" ]
                [ HH.text "No predicates found" ]
              else HH.div_ $ map renderPredicateItem predicates
            ]
        ]
    ]
  where
    renderPredicateItem pred =
      HH.button
        [ HP.style $ "display: block; width: 100%; text-align: left; "
            <> "padding: 10px 12px; margin: 4px 0; "
            <> "background: #f5f5f5; border: 1px solid #ddd; border-radius: 4px; "
            <> "cursor: pointer; font-family: monospace; font-size: 14px;"
        , HE.onClick \_ -> SelectPredicate pred
        ]
        [ HH.text $ pred.name <> "/" <> show pred.arity ]

-- | Render the modal showing predicate references
renderPredicateModal :: forall m.
  Maybe ASP.Predicate ->
  Array { name :: String, content :: String } ->
  (ASP.Predicate -> Array ASP.PredicateRef) ->
  H.ComponentHTML Action Slots m
renderPredicateModal Nothing _ _ = HH.text ""
renderPredicateModal (Just pred) _ findRefs =
  let refs = findRefs pred
  in
  HH.div
    [ HP.style $ "position: fixed; top: 0; left: 0; right: 0; bottom: 0; "
        <> "background: rgba(0,0,0,0.5); z-index: 200; "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "padding: 20px;"
    , HE.onClick \_ -> ClosePredicateModal
    ]
    [ HH.div
        [ HP.style $ "background: white; border-radius: 8px; "
            <> "max-width: 800px; width: 100%; max-height: 80vh; "
            <> "display: flex; flex-direction: column; "
            <> "box-shadow: 0 4px 20px rgba(0,0,0,0.3);"
        , HE.onClick \_ -> NoOp  -- Prevent clicks inside modal from closing it
        ]
        [ -- Modal header
          HH.div
            [ HP.style "padding: 15px 20px; background: #2196F3; color: white; border-radius: 8px 8px 0 0; display: flex; justify-content: space-between; align-items: center;" ]
            [ HH.strong_ [ HH.text $ pred.name <> "/" <> show pred.arity ]
            , HH.button
                [ HP.style "background: transparent; border: none; color: white; font-size: 24px; cursor: pointer; padding: 0 5px;"
                , HE.onClick \_ -> ClosePredicateModal
                ]
                [ HH.text "×" ]
            ]
        -- Modal body with references
        , HH.div
            [ HP.style "flex: 1; overflow-y: auto; padding: 15px;" ]
            [ if null refs
              then HH.p
                [ HP.style "color: #666; font-style: italic;" ]
                [ HH.text "No references found" ]
              else HH.div_ $ map renderReference refs
            ]
        -- Modal footer
        , HH.div
            [ HP.style "padding: 10px 20px; background: #f5f5f5; border-radius: 0 0 8px 8px; text-align: right;" ]
            [ HH.text $ show (length refs) <> " reference(s) found" ]
        ]
    ]
  where
    renderReference (ASP.PredicateRef ref) =
      HH.button
        [ HP.style $ "display: block; width: 100%; text-align: left; margin: 8px 0; padding: 10px; "
            <> "background: #fafafa; border: none; border-left: 3px solid #2196F3; "
            <> "border-radius: 0 4px 4px 0; cursor: pointer; "
            <> "transition: background-color 0.2s;"
        , HE.onClick \_ -> JumpToReference ref.sourceFile ref.lineNumber
        ]
        [ HH.div
            [ HP.style "font-size: 12px; color: #2196F3; margin-bottom: 5px; font-weight: bold;" ]
            [ HH.text $ ref.sourceFile <> " : line " <> show ref.lineNumber <> " (tap to go)" ]
        , HH.code
            [ HP.style "display: block; font-family: monospace; font-size: 13px; white-space: pre-wrap; color: #333;" ]
            [ HH.text ref.lineContent ]
        ]

-- | Render the dialog for navigating to an included file
renderNavigateIncludeDialog :: forall m. Maybe String -> Map.Map String String -> H.ComponentHTML Action Slots m
renderNavigateIncludeDialog Nothing _ = HH.text ""
renderNavigateIncludeDialog (Just targetPath) files =
  let
    fileExists = Map.member targetPath files
  in
  HH.div
    [ HP.style $ "position: fixed; top: 0; left: 0; right: 0; bottom: 0; "
        <> "background: rgba(0,0,0,0.5); z-index: 300; "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "padding: 20px;"
    , HE.onClick \_ -> CancelNavigateToInclude
    ]
    [ HH.div
        [ HP.style $ "background: white; border-radius: 8px; "
            <> "max-width: 450px; width: 100%; "
            <> "box-shadow: 0 4px 20px rgba(0,0,0,0.3);"
        , HE.onClick \_ -> NoOp  -- Prevent clicks inside dialog from closing it
        ]
        [ -- Dialog header
          HH.div
            [ HP.style "padding: 15px 20px; background: #FF9800; color: white; border-radius: 8px 8px 0 0;" ]
            [ HH.strong_ [ HH.text "Navigate to Included File" ] ]
        -- Dialog body
        , HH.div
            [ HP.style "padding: 20px;" ]
            [ HH.p
                [ HP.style "margin: 0 0 15px 0; color: #333;" ]
                [ HH.text "Do you want to open the included file?" ]
            , HH.div
                [ HP.style $ "padding: 12px; background: #f5f5f5; border-radius: 4px; "
                    <> "font-family: monospace; font-size: 14px; "
                    <> "border: 1px solid " <> (if fileExists then "#4CAF50" else "#f44336") <> ";"
                ]
                [ HH.text targetPath ]
            , if not fileExists
                then HH.p
                  [ HP.style "margin: 15px 0 0 0; color: #f44336; font-size: 13px;" ]
                  [ HH.text "⚠ This file does not exist in the virtual filesystem." ]
                else HH.text ""
            ]
        -- Dialog footer
        , HH.div
            [ HP.style "padding: 15px 20px; background: #f5f5f5; border-radius: 0 0 8px 8px; display: flex; justify-content: flex-end; gap: 10px;" ]
            [ HH.button
                [ HP.style "padding: 8px 16px; font-size: 14px; cursor: pointer; background: #ccc; color: #333; border: none; border-radius: 4px;"
                , HE.onClick \_ -> CancelNavigateToInclude
                ]
                [ HH.text "Cancel" ]
            , HH.button
                [ HP.style $ "padding: 8px 16px; font-size: 14px; cursor: pointer; "
                    <> "background: " <> (if fileExists then "#4CAF50" else "#ccc") <> "; "
                    <> "color: white; border: none; border-radius: 4px;"
                , HE.onClick \e -> ConfirmNavigateToInclude e targetPath
                , HP.disabled (not fileExists)
                ]
                [ HH.text "Open File" ]
            ]
        ]
    ]

module Component.ClingoDemo where

import Prelude

import AnswerSetParser as AnswerSet
import AspParser as ASP
import BuildInfo as BI
import Clingo as Clingo
import Data.Map as Map
import Data.Set as Set
import Component.TimelineGrimoire as TG
import FilterExpression as FE
import Data.Array (filter, fromFoldable, index, length, mapWithIndex, nub, null, slice, sort, sortBy)
import Data.Foldable (foldl, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String (Pattern(..), split, trim, take) as String
import Data.String (Pattern(..), split, trim)
import Effect.Class (liftEffect)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import EmbeddedPrograms as EP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TextareaUtils as TU
import Type.Proxy (Proxy(..))

-- | Child slots for embedded components
type Slots = ( timelineGrimoire :: H.Slot TG.Query TG.Output Unit )

_timelineGrimoire :: Proxy "timelineGrimoire"
_timelineGrimoire = Proxy

-- | Component state
type State =
  { files :: Map.Map String String  -- Virtual filesystem: filepath -> content
  , currentFile :: String           -- Currently selected file for editing (full path)
  , showFileDirectory :: Boolean    -- Is file directory popup visible
  , expandedDirs :: Set.Set String  -- Which directories are expanded in the tree view
  , modelLimit :: String            -- Max models to return (empty = 5, 0 = all)
  , result :: Maybe ResultDisplay
  , isLoading :: Boolean
  , isInitialized :: Boolean
  , selectedModelIndex :: Int       -- Which model to show in Timeline/Grimoire (0-indexed)
  -- Pagination for answer sets list (prevents browser crash with many models)
  , answerSetPage :: Int            -- Current page of answer sets (0-indexed)
  -- Predicate navigator state
  , showPredicateList :: Boolean
  , selectedPredicate :: Maybe ASP.Predicate
  -- Output filter expression (for filtering displayed atoms)
  , outputFilter :: String          -- Boolean expression to filter atoms
  -- Scroll notification (shown briefly when timeline click can't find the atom)
  , scrollNotification :: Maybe String
  }

-- | How to display results
data ResultDisplay
  = ResultSuccess (Array (Array String)) -- Answer sets
  | ResultUnsat
  | ResultError String

-- | Component actions
data Action
  = Initialize
  | SelectFile String           -- Switch to editing a different file (full path)
  | SetFileContent String       -- Update current file's content
  | ToggleFileDirectory         -- Show/hide file directory popup
  | ToggleDirectory String      -- Expand/collapse a directory in tree view
  | SetModelLimit String
  | SetOutputFilter String      -- Update output filter expression
  | RunClingo
  | CancelSolve
  | SelectModel Int             -- Select which model to display in Timeline/Grimoire
  | PrevAnswerSetPage
  | NextAnswerSetPage
  | TogglePredicateList
  | SelectPredicate ASP.Predicate
  | ClosePredicateModal
  | JumpToReference String Int  -- sourceFile, lineNumber
  | HandleTimelineEvent TG.Output  -- Handle timeline event clicks
  | ClearScrollNotification     -- Clear the scroll notification message
  | NoOp  -- Used to stop event propagation

-- | Number of answer sets to display per page (prevents browser crash with many models)
answerSetPageSize :: Int
answerSetPageSize = 20

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

-- | Get current file content from state
getCurrentFileContent :: State -> String
getCurrentFileContent state = fromMaybe "" $ Map.lookup state.currentFile state.files

-- | Build sources array from files Map for parsing
getSources :: State -> Array { name :: String, content :: String }
getSources state = map (\path -> { name: path, content: fromMaybe "" $ Map.lookup path state.files }) availableFiles

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
        , -- Editor textarea
          HH.textarea
            [ HP.style $ "width: 100%; height: 400px; font-family: monospace; font-size: 12px; "
                <> "padding: 10px; border: 1px solid #ccc; border-radius: 4px; "
                <> "resize: vertical; overflow: auto;"
            , HP.id "editor-textarea"
            , HP.value currentContent
            , HE.onValueInput SetFileContent
            , HP.disabled state.isLoading
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

    -- Results display
    , renderResult state

    -- File directory popup with tree view
    , renderFileDirectory state.showFileDirectory state.currentFile state.expandedDirs

    -- Predicate list panel (slide-in from right)
    , renderPredicatePanel state.showPredicateList parsed.predicates

    -- Modal for predicate references
    , renderPredicateModal state.selectedPredicate sources parsed.references
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

-- | Render the result section
renderResult :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
renderResult state = case state.result of
  Nothing ->
    HH.div
      [ HP.style "padding: 20px; background: #f5f5f5; border-radius: 4px; color: #666;" ]
      [ HH.text "Click 'Run Clingo' to solve the program" ]

  Just (ResultError err) ->
    HH.div
      [ HP.style "padding: 20px; background: #ffebee; border-radius: 4px; color: #c62828; font-family: monospace; white-space: pre-wrap;" ]
      [ HH.strong_ [ HH.text "Error: " ]
      , HH.text err
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
      selectedSet = index answerSets selectedIdx
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
    HH.div_
      [ -- Timeline and Grimoire view FIRST (for selected model)
        case selectedSet of
          Just atoms ->
            HH.div
              [ HP.style "margin-bottom: 20px;" ]
              [ HH.h2
                  [ HP.style "color: #333; margin-bottom: 10px;" ]
                  [ HH.text $ "Timeline & Grimoire View"
                      <> if totalCount > 1
                         then " (Model " <> show (selectedIdx + 1) <> " of " <> show totalCount <> ")"
                         else ""
                  ]
              , HH.p
                  [ HP.style "font-size: 12px; color: #666; margin-bottom: 10px; font-style: italic;" ]
                  [ HH.text "Click on timeline events to highlight the corresponding atom in the answer set below." ]
              , HH.slot _timelineGrimoire unit TG.component atoms HandleTimelineEvent
              ]
          Nothing -> HH.text ""
      -- Output section SECOND (compact, scrollable)
      , HH.div
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
      ]
    where
      renderAnswerSet filterExpr selectedIdx idx atoms =
        let
          isSelected = idx == selectedIdx
          -- Apply filter to atoms
          filteredAtoms = FE.filterAtoms filterExpr atoms
          totalAtomCount = length atoms
          filteredAtomCount = length filteredAtoms
          isFiltered = filterExpr /= "" && filteredAtomCount /= totalAtomCount
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
              [ HP.style $ "font-weight: bold; margin-bottom: 4px; font-size: 12px; "
                  <> if isSelected then "color: #1b5e20;" else "color: #388e3c;"
              ]
              [ HH.text $ "Answer Set " <> show (idx + 1)
                  <> (if isSelected then " ✓" else "")
                  <> (if isFiltered then " (" <> show filteredAtomCount <> "/" <> show totalAtomCount <> " atoms)" else "")
              ]
          , HH.code
              [ HP.style $ "display: block; font-family: monospace; white-space: pre-wrap; color: #1b5e20; font-size: 11px; max-height: 60px; overflow-y: auto; "
                  <> "-webkit-overflow-scrolling: touch;"
              ]
              [ HH.text $ intercalate " " filteredAtoms ]
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

-- | Handle component actions
handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize clingo-wasm (relative path works locally and on GitHub Pages)
    H.liftAff $ Clingo.init "./clingo.wasm"
    H.modify_ \s -> s { isInitialized = true }

  SelectFile fileName -> do
    H.modify_ \s -> s { currentFile = fileName, showFileDirectory = false }

  SetFileContent content -> do
    H.modify_ \s -> s { files = Map.insert s.currentFile content s.files }

  ToggleFileDirectory ->
    H.modify_ \s -> s { showFileDirectory = not s.showFileDirectory }

  ToggleDirectory dir -> do
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
    -- Build file resolver for #include directives using the virtual filesystem
    let resolver filename = Map.lookup filename state.files
    -- Get the current file's content (the entry point that #includes other files)
    let entryFile = state.currentFile
    let entryProgram = fromMaybe "" $ Map.lookup entryFile state.files
    -- Resolve #include directives recursively, using path-aware resolution
    -- This follows Clingo's behavior: try CWD first, then relative to including file
    let fullProgram = Clingo.resolveIncludesWithPath entryProgram entryFile resolver
    result <- H.liftAff $ Clingo.run fullProgram numModels
    let display = case result of
          Clingo.Satisfiable res ->
            ResultSuccess $ extractWitnesses res
          Clingo.OptimumFound res ->
            ResultSuccess $ extractWitnesses res
          Clingo.Unsatisfiable _ ->
            ResultUnsat
          Clingo.Unknown _ ->
            ResultError "Result unknown"
          Clingo.Error err ->
            ResultError err
    H.modify_ \s -> s { isLoading = false, result = Just display }

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
                  Aff.delay (Milliseconds 3000.0)
                  H.modify_ \s -> s { scrollNotification = Nothing }
                pure unit
            else do
              -- Notify user that the model is not on the current page
              H.modify_ \s -> s { scrollNotification = Just $ "Model " <> show (selectedIdx + 1) <> " is not on the current page" }
              _ <- H.fork do
                Aff.delay (Milliseconds 3000.0)
                H.modify_ \s -> s { scrollNotification = Nothing }
              pure unit
        else do
          -- Empty sourceAtom - this shouldn't normally happen
          H.modify_ \s -> s { scrollNotification = Just $ "No source atom for " <> predicateName <> " event" }
          _ <- H.fork do
            Aff.delay (Milliseconds 3000.0)
            H.modify_ \s -> s { scrollNotification = Nothing }
          pure unit
    TG.ReminderMoved { token, fromPlayer, toPlayer, time } -> do
      -- Modify inst.lp to add a constraint for the new reminder placement
      state <- H.get
      let instContent = fromMaybe "" $ Map.lookup "inst.lp" state.files
      -- Format the time point for ASP
      let timeStr = formatTimePointForASP time
      -- Create the new constraint
      let newConstraint = ":- not reminder_on(" <> token <> ", " <> toPlayer <> ", " <> timeStr <> ")."
      -- Create the old constraint pattern to comment out
      let oldConstraintPattern = ":- not reminder_on(" <> token <> ", " <> fromPlayer <> ", " <> timeStr <> ")."
      -- Modify the content: comment out old constraint if present, add new one
      let modifiedContent = modifyInstLpForReminder instContent oldConstraintPattern newConstraint
      -- Update the virtual filesystem
      H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files }
      -- Re-run Clingo with the new constraints
      handleAction RunClingo
    TG.RoleMoved { role, fromPlayer, toPlayer, time } -> do
      -- Modify inst.lp to add a constraint for the new role assignment
      state <- H.get
      let instContent = fromMaybe "" $ Map.lookup "inst.lp" state.files
      -- Convert time to an integer for assigned/3 predicate
      -- For Night 1 (setup), use 0; otherwise use night number
      let timeIdx = timePointToAssignedTime time
      -- Create the new constraint (forces toPlayer to have this role at this time)
      let newConstraint = ":- not assigned(" <> show timeIdx <> ", " <> toPlayer <> ", " <> role <> ")."
      -- Create the old constraint pattern to comment out (the previous drag assignment for this role)
      let oldConstraintPattern = ":- not assigned(" <> show timeIdx <> ", " <> fromPlayer <> ", " <> role <> ")."
      -- Modify the content: comment out old constraint if present, add new one
      let modifiedContent = modifyInstLpForRole instContent oldConstraintPattern newConstraint
      -- Update the virtual filesystem
      H.modify_ \s -> s { files = Map.insert "inst.lp" modifiedContent s.files }
      -- Re-run Clingo with the new constraints
      handleAction RunClingo

  ClearScrollNotification ->
    H.modify_ \s -> s { scrollNotification = Nothing }

  NoOp ->
    pure unit  -- Do nothing, used to stop event propagation

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

-- | Format a TimePoint for ASP syntax
formatTimePointForASP :: AnswerSet.TimePoint -> String
formatTimePointForASP (AnswerSet.Night n r s) = "night(" <> show n <> ", " <> show r <> ", " <> show s <> ")"
formatTimePointForASP (AnswerSet.Day n phase) = "day(" <> show n <> ", " <> phase <> ")"
formatTimePointForASP (AnswerSet.UnknownTime s) = s

-- | Modify inst.lp to add a new reminder constraint and comment out conflicting one
modifyInstLpForReminder :: String -> String -> String -> String
modifyInstLpForReminder content oldPattern newConstraint =
  let
    -- Split content into lines
    contentLines = String.split (String.Pattern "\n") content
    -- Comment out any existing line matching the old pattern
    modifiedLines = map (commentOutIfMatches oldPattern) contentLines
    -- Check if the new constraint already exists
    hasNewConstraint = foldl (\acc line -> acc || trim line == trim newConstraint) false modifiedLines
    -- Add the new constraint at the end if not already present
    finalLines = if hasNewConstraint
                   then modifiedLines
                   else modifiedLines <> ["", "% Reminder token constraint (added by drag)", newConstraint]
  in
    intercalate "\n" finalLines

-- | Comment out a line if it matches the pattern (case-sensitive, trimmed comparison)
commentOutIfMatches :: String -> String -> String
commentOutIfMatches patternStr line =
  let
    trimmedLine = trim line
    trimmedPattern = trim patternStr
    firstChar = String.take 1 trimmedLine
  in
    if trimmedLine == trimmedPattern && firstChar /= "%"
      then "% " <> line <> "  % commented out by drag"
      else line

-- | Convert a TimePoint to an integer time index for assigned/3 predicate
-- Night 1 (any phase) maps to 0 (initial assignment)
-- Other nights map to their night number
timePointToAssignedTime :: AnswerSet.TimePoint -> Int
timePointToAssignedTime (AnswerSet.Night 1 _ _) = 0
timePointToAssignedTime (AnswerSet.Night n _ _) = n
timePointToAssignedTime (AnswerSet.Day n _) = n
timePointToAssignedTime (AnswerSet.UnknownTime _) = 0

-- | Modify inst.lp to add a new role assignment constraint and comment out conflicting one
modifyInstLpForRole :: String -> String -> String -> String
modifyInstLpForRole content oldPattern newConstraint =
  let
    -- Split content into lines
    contentLines = String.split (String.Pattern "\n") content
    -- Comment out any existing line matching the old pattern
    modifiedLines = map (commentOutIfMatches oldPattern) contentLines
    -- Check if the new constraint already exists
    hasNewConstraint = foldl (\acc line -> acc || trim line == trim newConstraint) false modifiedLines
    -- Add the new constraint at the end if not already present
    finalLines = if hasNewConstraint
                   then modifiedLines
                   else modifiedLines <> ["", "% Role assignment constraint (added by drag)", newConstraint]
  in
    intercalate "\n" finalLines

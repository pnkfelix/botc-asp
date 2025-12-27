module Component.ClingoDemo where

import Prelude

import AspParser as ASP
import Clingo as Clingo
import Data.Map as Map
import Component.TimelineGrimoire as TG
import Data.Array (index, length, mapWithIndex, null, slice)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String (trim)
import Effect.Class (liftEffect)
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
  { files :: Map.Map String String  -- Virtual filesystem: filename -> content
  , currentFile :: String           -- Currently selected file for editing
  , showFileDirectory :: Boolean    -- Is file directory popup visible
  , modelLimit :: String            -- Max models to return (empty = 0 = all)
  , result :: Maybe ResultDisplay
  , isLoading :: Boolean
  , isInitialized :: Boolean
  , selectedModelIndex :: Int       -- Which model to show in Timeline/Grimoire (0-indexed)
  -- Pagination for answer sets list (prevents browser crash with many models)
  , answerSetPage :: Int            -- Current page of answer sets (0-indexed)
  -- Predicate navigator state
  , showPredicateList :: Boolean
  , selectedPredicate :: Maybe ASP.Predicate
  }

-- | How to display results
data ResultDisplay
  = ResultSuccess (Array (Array String)) -- Answer sets
  | ResultUnsat
  | ResultError String

-- | Component actions
data Action
  = Initialize
  | SelectFile String           -- Switch to editing a different file
  | SetFileContent String       -- Update current file's content
  | ToggleFileDirectory         -- Show/hide file directory popup
  | SetModelLimit String
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
  | NoOp  -- Used to stop event propagation

-- | Number of answer sets to display per page (prevents browser crash with many models)
answerSetPageSize :: Int
answerSetPageSize = 20

-- | List of available files in order
availableFiles :: Array String
availableFiles = ["inst.lp", "botc.lp", "tb.lp", "players.lp", "types.lp"]

-- | Initial state with embedded .lp file contents
initialState :: State
initialState =
  { files: Map.fromFoldable
      [ Tuple "botc.lp" EP.botcLp
      , Tuple "tb.lp" EP.tbLp
      , Tuple "players.lp" EP.playersLp
      , Tuple "inst.lp" EP.instLp
      , Tuple "types.lp" EP.typesLp
      ]
  , currentFile: "inst.lp"  -- Start with instance file selected
  , showFileDirectory: false
  , modelLimit: ""  -- Empty = 0 = all models
  , result: Nothing
  , isLoading: false
  , isInitialized: false
  , selectedModelIndex: 0  -- First model selected by default
  , answerSetPage: 0       -- First page of answer sets
  , showPredicateList: false
  , selectedPredicate: Nothing
  }

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
getSources state = map (\name -> { name, content: fromMaybe "" $ Map.lookup name state.files }) availableFiles

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

    -- Single file editor with file tabs
    , HH.div
        [ HP.style "margin: 20px 0;" ]
        [ -- File tabs
          HH.div
            [ HP.style "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 8px;" ]
            (map (renderFileTab state.currentFile) availableFiles)
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
                , HP.placeholder "0 = all"
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

    -- File directory popup
    , renderFileDirectory state.showFileDirectory state.currentFile

    -- Predicate list panel (slide-in from right)
    , renderPredicatePanel state.showPredicateList parsed.predicates

    -- Modal for predicate references
    , renderPredicateModal state.selectedPredicate sources parsed.references
    ]

-- | Render a file tab button
renderFileTab :: forall m. String -> String -> H.ComponentHTML Action Slots m
renderFileTab currentFile fileName =
  let
    isSelected = fileName == currentFile
    baseStyle = "padding: 8px 16px; font-size: 14px; cursor: pointer; border: none; border-radius: 4px 4px 0 0; "
    selectedStyle = if isSelected
      then "background: #4CAF50; color: white; font-weight: bold;"
      else "background: #e0e0e0; color: #333;"
  in
  HH.button
    [ HP.style $ baseStyle <> selectedStyle
    , HE.onClick \_ -> SelectFile fileName
    ]
    [ HH.text fileName ]

-- | Render the file directory popup
renderFileDirectory :: forall m. Boolean -> String -> H.ComponentHTML Action Slots m
renderFileDirectory isVisible currentFile =
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
            <> "min-width: 300px; "
            <> "opacity: " <> (if isVisible then "1" else "0") <> "; "
            <> "pointer-events: " <> (if isVisible then "auto" else "none") <> "; "
            <> "transition: opacity 0.3s ease;"
        , HE.onClick \_ -> NoOp  -- Prevent clicks from closing via backdrop
        ]
        [ -- Header
          HH.div
            [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;" ]
            [ HH.h3
                [ HP.style "margin: 0; color: #333;" ]
                [ HH.text "Files" ]
            , HH.button
                [ HP.style "background: transparent; border: none; font-size: 20px; cursor: pointer; padding: 0 5px;"
                , HE.onClick \_ -> ToggleFileDirectory
                ]
                [ HH.text "×" ]
            ]
        -- File list
        , HH.div
            [ HP.style "display: flex; flex-direction: column; gap: 8px;" ]
            (map (renderFileItem currentFile) availableFiles)
        ]
    ]

-- | Render a file item in the directory popup
renderFileItem :: forall m. String -> String -> H.ComponentHTML Action Slots m
renderFileItem currentFile fileName =
  let
    isSelected = fileName == currentFile
    description = case fileName of
      "inst.lp" -> "Instance/scenario configuration"
      "botc.lp" -> "Core game rules"
      "tb.lp" -> "Trouble Brewing script"
      "players.lp" -> "Player names and seating"
      "types.lp" -> "Type validation rules"
      _ -> ""
  in
  HH.button
    [ HP.style $ "display: block; width: 100%; text-align: left; "
        <> "padding: 12px 15px; "
        <> "background: " <> (if isSelected then "#e8f5e9" else "#f5f5f5") <> "; "
        <> "border: " <> (if isSelected then "2px solid #4CAF50" else "1px solid #ddd") <> "; "
        <> "border-radius: 4px; cursor: pointer;"
    , HE.onClick \_ -> SelectFile fileName
    ]
    [ HH.div
        [ HP.style $ "font-weight: " <> (if isSelected then "bold" else "normal") <> "; "
            <> "font-family: monospace; font-size: 14px;"
        ]
        [ HH.text fileName ]
    , HH.div
        [ HP.style "font-size: 12px; color: #666; margin-top: 4px;" ]
        [ HH.text description ]
    ]

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
          , HH.div
              [ HP.id "answer-set-display"
              , HP.style $ "max-height: 200px; overflow-y: scroll; margin-top: 10px; "
                  <> "-webkit-overflow-scrolling: touch; overscroll-behavior: contain; "
                  <> "touch-action: pan-y;"
              ]
              -- Render only the current page of answer sets (with correct global indices)
              [ HH.div_ $ mapWithIndex (\pageIdx atoms -> renderAnswerSet selectedIdx (pageStart + pageIdx) atoms) pageItems ]
          ]
      ]
    where
      renderAnswerSet selectedIdx idx atoms =
        let
          isSelected = idx == selectedIdx
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
              [ HH.text $ "Answer Set " <> show (idx + 1) <> (if isSelected then " ✓" else "") ]
          , HH.code
              [ HP.style $ "display: block; font-family: monospace; white-space: pre-wrap; color: #1b5e20; font-size: 11px; max-height: 60px; overflow-y: auto; "
                  <> "-webkit-overflow-scrolling: touch;"
              ]
              [ HH.text $ intercalate " " atoms ]
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

  SetModelLimit limit ->
    H.modify_ \s -> s { modelLimit = limit }

  RunClingo -> do
    H.modify_ \s -> s { isLoading = true, result = Nothing, selectedModelIndex = 0, answerSetPage = 0 }
    state <- H.get
    -- Parse model limit (empty or invalid = 0 = all models)
    let numModels = fromMaybe 0 $ Int.fromString (trim state.modelLimit)
    -- Build file resolver for #include directives using the virtual filesystem
    let resolver filename = Map.lookup filename state.files
    -- Get inst.lp content (the entry point that #includes other files)
    let instProgram = fromMaybe "" $ Map.lookup "inst.lp" state.files
    -- Resolve #include directives recursively
    let fullProgram = Clingo.resolveIncludes instProgram resolver
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
    TG.TimelineEventClicked { sourceAtom } -> do
      -- Scroll to the atom in the answer set display
      if sourceAtom /= ""
        then do
          _ <- liftEffect $ TU.scrollToText "answer-set-display" sourceAtom
          pure unit
        else pure unit

  NoOp ->
    pure unit  -- Do nothing, used to stop event propagation

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

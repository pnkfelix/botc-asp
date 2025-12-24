module Component.ClingoDemo where

import Prelude

import AspParser as ASP
import Clingo as Clingo
import Component.TimelineGrimoire as TG
import Data.Array (length, mapWithIndex, null, head)
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Void (Void, absurd)
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

-- | Which program panel is being edited
data ProgramPanel = BotcPanel | TbPanel | PlayersPanel | InstancePanel

derive instance eqProgramPanel :: Eq ProgramPanel

-- | Child slots for embedded components
type Slots = ( timelineGrimoire :: H.Slot TG.Query Void Unit )

_timelineGrimoire :: Proxy "timelineGrimoire"
_timelineGrimoire = Proxy

-- | Component state
type State =
  { botcProgram :: String      -- Core game rules (botc.lp)
  , tbProgram :: String        -- Trouble Brewing script (tb.lp)
  , playersProgram :: String   -- Player configuration (players.lp)
  , instanceProgram :: String  -- Instance/query for this run
  , modelLimit :: String       -- Max models to return (empty = 0 = all)
  , result :: Maybe ResultDisplay
  , isLoading :: Boolean
  , isInitialized :: Boolean
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
  | SetProgram ProgramPanel String
  | SetModelLimit String
  | RunClingo
  | CancelSolve
  | TogglePredicateList
  | SelectPredicate ASP.Predicate
  | ClosePredicateModal
  | JumpToReference String Int  -- sourceFile, lineNumber
  | NoOp  -- Used to stop event propagation

-- | Initial state with embedded .lp file contents
initialState :: State
initialState =
  { botcProgram: EP.botcLp
  , tbProgram: EP.tbLp
  , playersProgram: defaultPlayers  -- Use smaller player set for demo
  , instanceProgram: defaultInstance
  , modelLimit: ""  -- Empty = 0 = all models
  , result: Nothing
  , isLoading: false
  , isInitialized: false
  , showPredicateList: false
  , selectedPredicate: Nothing
  }

-- | Smaller player configuration for demo (8 players instead of 16)
defaultPlayers :: String
defaultPlayers = """% Player names and seating (8 players for demo)
name(amanda; rob; taylor; courtney; steph; felix; neha; pratik).

chair(amanda, 0).
chair(rob, 1).
chair(taylor, 2).
chair(courtney, 3).
chair(steph, 4).
chair(felix, 5).
chair(neha, 6).
chair(pratik, 7).

% Number of players in the game
#const player_count = 8.
"""

-- | Instance/demo fragment
defaultInstance :: String
defaultInstance = """% Instance: constraints and queries for this specific scenario
% Edit this section to explore different game states

% Example: Constrain who the demon might be
% assigned(0, felix, imp).

% Example: Constrain what the Chef learned
% st_tells(chef, amanda, count(1), night(1, 5, 1)).

% Show key derived facts
#show assigned/3.
#show received/2.
#show st_tells/4.

% Show additional predicates for timeline/grimoire view
#show player_chooses/4.
#show reminder_on/3.
#show time/1.
#show alive/2.
#show chair/2.
"""

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

-- | Render the component
render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  let
    -- Parse all programs to get predicates
    sources =
      [ { name: "botc.lp", content: state.botcProgram }
      , { name: "tb.lp", content: state.tbProgram }
      , { name: "players.lp", content: state.playersProgram }
      , { name: "instance", content: state.instanceProgram }
      ]
    parsed = ASP.parseProgram sources
  in
  HH.div
    [ HP.style "font-family: system-ui, sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px; position: relative;" ]
    [ HH.h1_ [ HH.text "Clingo WASM + PureScript Demo" ]
    , HH.p
        [ HP.style "color: #666;" ]
        [ HH.text "Blood on the Clocktower ASP Explorer" ]

    -- Predicate navigator toggle button (mobile-friendly)
    , HH.button
        [ HP.style $ "position: fixed; bottom: 20px; right: 20px; z-index: 100; "
            <> "padding: 12px 16px; font-size: 14px; cursor: pointer; "
            <> "background: #2196F3; color: white; border: none; border-radius: 50px; "
            <> "box-shadow: 0 2px 8px rgba(0,0,0,0.3);"
        , HE.onClick \_ -> TogglePredicateList
        ]
        [ HH.text $ if state.showPredicateList then "Hide Predicates" else "Show Predicates" ]

    -- Program panels in a 2x2 grid (responsive)
    , HH.div
        [ HP.style "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px; margin: 20px 0;" ]
        [ renderProgramPanel "botc.lp (Core Rules)" BotcPanel state.botcProgram state.isLoading
        , renderProgramPanel "tb.lp (Trouble Brewing)" TbPanel state.tbProgram state.isLoading
        , renderProgramPanel "players.lp (Players)" PlayersPanel state.playersProgram state.isLoading
        , renderProgramPanel "Instance (Query)" InstancePanel state.instanceProgram state.isLoading
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
    , renderResult state.result

    -- Predicate list panel (slide-in from right)
    , renderPredicatePanel state.showPredicateList parsed.predicates

    -- Modal for predicate references
    , renderPredicateModal state.selectedPredicate sources parsed.references
    ]

-- | Render a single program panel with label and scrollable textarea
renderProgramPanel :: forall m. String -> ProgramPanel -> String -> Boolean -> H.ComponentHTML Action Slots m
renderProgramPanel label panel value isLoading =
  HH.div
    [ HP.style "display: flex; flex-direction: column;" ]
    [ HH.label
        [ HP.style "font-weight: bold; margin-bottom: 8px; color: #333;" ]
        [ HH.text label ]
    , HH.textarea
        [ HP.style $ "width: 100%; height: 300px; font-family: monospace; font-size: 12px; "
            <> "padding: 10px; border: 1px solid #ccc; border-radius: 4px; "
            <> "resize: vertical; overflow: auto;"
        , HP.id (panelToTextareaId panel)
        , HP.value value
        , HE.onValueInput (SetProgram panel)
        , HP.disabled isLoading
        ]
    ]

-- | Convert a panel to its textarea element ID
panelToTextareaId :: ProgramPanel -> String
panelToTextareaId = case _ of
  BotcPanel -> "textarea-botc"
  TbPanel -> "textarea-tb"
  PlayersPanel -> "textarea-players"
  InstancePanel -> "textarea-instance"

-- | Convert a source file name to textarea ID
sourceFileToTextareaId :: String -> Maybe String
sourceFileToTextareaId = case _ of
  "botc.lp" -> Just "textarea-botc"
  "tb.lp" -> Just "textarea-tb"
  "players.lp" -> Just "textarea-players"
  "instance" -> Just "textarea-instance"
  _ -> Nothing

-- | Render the result section
renderResult :: forall m. MonadAff m => Maybe ResultDisplay -> H.ComponentHTML Action Slots m
renderResult Nothing =
  HH.div
    [ HP.style "padding: 20px; background: #f5f5f5; border-radius: 4px; color: #666;" ]
    [ HH.text "Click 'Run Clingo' to solve the program" ]

renderResult (Just (ResultError err)) =
  HH.div
    [ HP.style "padding: 20px; background: #ffebee; border-radius: 4px; color: #c62828; font-family: monospace; white-space: pre-wrap;" ]
    [ HH.strong_ [ HH.text "Error: " ]
    , HH.text err
    ]

renderResult (Just ResultUnsat) =
  HH.div
    [ HP.style "padding: 20px; background: #fff3e0; border-radius: 4px; color: #e65100;" ]
    [ HH.strong_ [ HH.text "UNSATISFIABLE" ]
    , HH.p_ [ HH.text "No solutions exist for the given constraints." ]
    ]

renderResult (Just (ResultSuccess answerSets)) =
  HH.div_
    [ HH.div
        [ HP.style "padding: 20px; background: #e8f5e9; border-radius: 4px;" ]
        [ HH.strong
            [ HP.style "color: #2e7d32;" ]
            [ HH.text $ "SATISFIABLE - Found " <> show (length answerSets) <> " answer set(s)" ]
        , HH.div_ $ mapWithIndex renderAnswerSet answerSets
        ]
    -- Timeline and Grimoire view for the first answer set
    , case head answerSets of
        Just firstSet ->
          HH.div
            [ HP.style "margin-top: 20px;" ]
            [ HH.h2
                [ HP.style "color: #333; margin-bottom: 10px;" ]
                [ HH.text "Timeline & Grimoire View" ]
            , HH.slot _timelineGrimoire unit TG.component firstSet absurd
            ]
        Nothing -> HH.text ""
    ]
  where

    renderAnswerSet idx atoms =
      HH.div
        [ HP.style "margin-top: 15px; padding: 10px; background: white; border-radius: 4px; border: 1px solid #c8e6c9;" ]
        [ HH.div
            [ HP.style "font-weight: bold; color: #388e3c; margin-bottom: 8px;" ]
            [ HH.text $ "Answer Set " <> show (idx + 1) <> ":" ]
        , HH.code
            [ HP.style "display: block; font-family: monospace; white-space: pre-wrap; color: #1b5e20;" ]
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
renderPredicateModal (Just pred) sources findRefs =
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

  SetProgram panel prog ->
    H.modify_ \s -> case panel of
      BotcPanel -> s { botcProgram = prog }
      TbPanel -> s { tbProgram = prog }
      PlayersPanel -> s { playersProgram = prog }
      InstancePanel -> s { instanceProgram = prog }

  SetModelLimit limit ->
    H.modify_ \s -> s { modelLimit = limit }

  RunClingo -> do
    H.modify_ \s -> s { isLoading = true, result = Nothing }
    state <- H.get
    -- Parse model limit (empty or invalid = 0 = all models)
    let numModels = fromMaybe 0 $ Int.fromString (trim state.modelLimit)
    -- Concatenate all programs
    let fullProgram = state.botcProgram <> "\n\n"
                   <> state.tbProgram <> "\n\n"
                   <> state.playersProgram <> "\n\n"
                   <> state.instanceProgram
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

  TogglePredicateList ->
    H.modify_ \s -> s { showPredicateList = not s.showPredicateList }

  SelectPredicate pred ->
    H.modify_ \s -> s { selectedPredicate = Just pred }

  ClosePredicateModal ->
    H.modify_ \s -> s { selectedPredicate = Nothing }

  JumpToReference sourceFile lineNumber -> do
    -- Close modal and predicate panel
    H.modify_ \s -> s { selectedPredicate = Nothing, showPredicateList = false }
    -- Scroll to the line in the appropriate textarea
    case sourceFileToTextareaId sourceFile of
      Just textareaId -> liftEffect $ TU.scrollToLine textareaId lineNumber
      Nothing -> pure unit

  NoOp ->
    pure unit  -- Do nothing, used to stop event propagation

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

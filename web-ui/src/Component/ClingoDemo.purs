module Component.ClingoDemo where

import Prelude

import Clingo as Clingo
import Data.Array (intercalate, length, mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (trim)
import Effect.Aff.Class (class MonadAff)
import EmbeddedPrograms as EP
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Which program panel is being edited
data ProgramPanel = BotcPanel | TbPanel | PlayersPanel | InstancePanel

derive instance eqProgramPanel :: Eq ProgramPanel

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
render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.style "font-family: system-ui, sans-serif; max-width: 1400px; margin: 0 auto; padding: 20px;" ]
    [ HH.h1_ [ HH.text "Clingo WASM + PureScript Demo" ]
    , HH.p
        [ HP.style "color: #666;" ]
        [ HH.text "Blood on the Clocktower ASP Explorer" ]

    -- Program panels in a 2x2 grid
    , HH.div
        [ HP.style "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin: 20px 0;" ]
        [ renderProgramPanel "botc.lp (Core Rules)" BotcPanel state.botcProgram state.isLoading
        , renderProgramPanel "tb.lp (Trouble Brewing)" TbPanel state.tbProgram state.isLoading
        , renderProgramPanel "players.lp (Players)" PlayersPanel state.playersProgram state.isLoading
        , renderProgramPanel "Instance (Query)" InstancePanel state.instanceProgram state.isLoading
        ]

    -- Controls: model limit input, run button, cancel button
    , HH.div
        [ HP.style "margin: 20px 0; text-align: center; display: flex; justify-content: center; align-items: center; gap: 15px;" ]
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
    ]

-- | Render a single program panel with label and scrollable textarea
renderProgramPanel :: forall cs m. String -> ProgramPanel -> String -> Boolean -> H.ComponentHTML Action cs m
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
        , HP.value value
        , HE.onValueInput (SetProgram panel)
        , HP.disabled isLoading
        ]
    ]

-- | Render the result section
renderResult :: forall cs m. Maybe ResultDisplay -> H.ComponentHTML Action cs m
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
  HH.div
    [ HP.style "padding: 20px; background: #e8f5e9; border-radius: 4px;" ]
    [ HH.strong
        [ HP.style "color: #2e7d32;" ]
        [ HH.text $ "SATISFIABLE - Found " <> show (length answerSets) <> " answer set(s)" ]
    , HH.div_ $ mapWithIndex renderAnswerSet answerSets
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

-- | Handle component actions
handleAction :: forall cs o m. MonadAff m => Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    -- Initialize clingo-wasm
    H.liftAff $ Clingo.init "/clingo.wasm"
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
    H.liftAff $ Clingo.restart "/clingo.wasm"
    H.modify_ \s -> s { isLoading = false, result = Just (ResultError "Solve cancelled by user") }

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

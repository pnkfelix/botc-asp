module Component.ClingoDemo where

import Prelude

import Clingo as Clingo
import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- | Component state
type State =
  { program :: String
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
  | SetProgram String
  | RunClingo

-- | Default ASP program demonstrating Blood on the Clocktower concepts
defaultProgram :: String
defaultProgram = """% Simple BotC-style example: Who could be the Demon?
% Given: 3 players, one must be the Demon

player(alice). player(bob). player(charlie).

% Exactly one player is the demon
{ demon(P) : player(P) } = 1.

% The demon is evil
evil(P) :- demon(P).

% At least one player claims to be good
claims_good(alice).
claims_good(bob).

#show demon/1.
#show evil/1."""

-- | Initial state
initialState :: State
initialState =
  { program: defaultProgram
  , result: Nothing
  , isLoading: false
  , isInitialized: false
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

-- | Render the component
render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.style "font-family: system-ui, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px;" ]
    [ HH.h1_ [ HH.text "Clingo WASM + PureScript Demo" ]
    , HH.p
        [ HP.style "color: #666;" ]
        [ HH.text "Blood on the Clocktower ASP Explorer - FFI Spike" ]

    -- Program input
    , HH.div
        [ HP.style "margin: 20px 0;" ]
        [ HH.label
            [ HP.style "display: block; font-weight: bold; margin-bottom: 8px;" ]
            [ HH.text "ASP Program:" ]
        , HH.textarea
            [ HP.style "width: 100%; height: 300px; font-family: monospace; font-size: 14px; padding: 10px; border: 1px solid #ccc; border-radius: 4px;"
            , HP.value state.program
            , HE.onValueInput SetProgram
            , HP.disabled state.isLoading
            ]
        ]

    -- Run button
    , HH.div
        [ HP.style "margin: 20px 0;" ]
        [ HH.button
            [ HP.style $ "padding: 10px 20px; font-size: 16px; cursor: pointer; "
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
        ]

    -- Results display
    , renderResult state.result
    ]

-- | Render the result section
renderResult :: forall cs m. Maybe ResultDisplay -> H.ComponentHTML Action cs m
renderResult Nothing =
  HH.div
    [ HP.style "padding: 20px; background: #f5f5f5; border-radius: 4px; color: #666;" ]
    [ HH.text "Click 'Run Clingo' to solve the program" ]

renderResult (Just (ResultError err)) =
  HH.div
    [ HP.style "padding: 20px; background: #ffebee; border-radius: 4px; color: #c62828; font-family: monospace;" ]
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
    length = Data.Array.length
    mapWithIndex f arr = Data.Array.mapWithIndex f arr

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
    H.liftAff $ Clingo.init "./clingo.wasm"
    H.modify_ \s -> s { isInitialized = true }

  SetProgram prog ->
    H.modify_ \s -> s { program = prog }

  RunClingo -> do
    H.modify_ \s -> s { isLoading = true, result = Nothing }
    state <- H.get
    result <- H.liftAff $ Clingo.run state.program 0  -- 0 = all models
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

-- | Extract witnesses from a Clingo result
extractWitnesses :: Clingo.ClingoResult -> Array (Array String)
extractWitnesses res =
  res."Call" >>= \call -> call."Witnesses" <#> \w -> w."Value"

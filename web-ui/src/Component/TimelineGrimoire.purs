module Component.TimelineGrimoire
  ( component
  , Query
  , Output(..)
  ) where

import Prelude

import AnswerSetParser as ASP
import Data.Array (elem, filter, foldl, head, last, length, mapWithIndex, nub, null, sortBy, take, (..), findIndex)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (fold, intercalate, minimumBy)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Int (toNumber)
import Data.String (Pattern(..))
import Data.String.CodeUnits (toCharArray)
import Data.String as S
import Data.Number (sqrt, cos, sin, pi)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as SE
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Attributes.Color (Color(..))
import Halogen.Svg.Attributes.CSSLength (CSSLength(..))
import Halogen.Svg.Attributes.FontSize (FontSize(..))
import Halogen.Svg.Attributes.FontWeight (FontWeight(..))
import Halogen.Svg.Attributes.TextAnchor (TextAnchor(..))

import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import ReminderDrag as Drag
import RoleDrag as RoleDrag
import Halogen.Subscription as HS

-- | View mode for grimoire display
data ViewMode = SvgView | HtmlView

derive instance eqViewMode :: Eq ViewMode

-- | Output events from the component
data Output
  = TimelineEventClicked
      { sourceAtom :: String      -- Original atom string to highlight in answer set
      , predicateName :: String   -- Predicate name for finding rules (e.g., "d_st_tells")
      , predicateArity :: Int     -- Arity for finding rules
      }
  | ReminderMoved
      { token :: String           -- The reminder token being moved (e.g., "poi_poisoned")
      , fromPlayer :: String      -- Original player the token was on
      , toPlayer :: String        -- New player the token is being moved to
      , time :: ASP.TimePoint     -- The time point at which this reminder applies
      }
  | RoleMoved
      { role :: String            -- The role being moved (e.g., "drunk")
      , fromPlayer :: String      -- Original player who had the role
      , toPlayer :: String        -- New player receiving the role
      , time :: ASP.TimePoint     -- The time point at which this role assignment applies
      }

-- | Drag state for reminder tokens
type DragState =
  { reminder :: { token :: String, player :: String, placedAt :: ASP.TimePoint }
  , startX :: Number           -- Mouse X when drag started (SVG coords)
  , startY :: Number           -- Mouse Y when drag started (SVG coords)
  , currentX :: Number         -- Current mouse X (SVG coords)
  , currentY :: Number         -- Current mouse Y (SVG coords)
  , targetPlayer :: Maybe String  -- Player the token would be assigned to if dropped
  }

-- | Component state
type State =
  { parsedAtoms :: Array ASP.ParsedAtom  -- Parsed atoms with original strings
  , atoms :: Array ASP.Atom              -- Just the atoms (for backward compat)
  , timeline :: Array ASP.TimelineEvent
  , selectedTime :: Maybe ASP.TimePoint
  , allTimePoints :: Array ASP.TimePoint
  , dragging :: Maybe DragState          -- Current drag operation
  , svgBounds :: Maybe { left :: Number, top :: Number, width :: Number, height :: Number }
  , viewMode :: ViewMode                 -- SVG (circular) or HTML (grid) view
  , bagCollapsed :: Boolean              -- Whether the bag panel is collapsed
  , scriptCollapsed :: Boolean           -- Whether the script panel is collapsed
  }

-- | Trouble Brewing script roles by category
tbTownsfolk :: Array String
tbTownsfolk = ["washerwoman", "librarian", "investigator", "chef", "empath", "fortune_teller", "undertaker", "monk", "ravenkeeper", "virgin", "slayer", "soldier", "mayor"]

tbOutsiders :: Array String
tbOutsiders = ["butler", "drunk", "recluse", "saint"]

tbMinions :: Array String
tbMinions = ["poisoner", "spy", "scarlet_woman", "baron"]

tbDemons :: Array String
tbDemons = ["imp"]

-- | Component query type (empty - no queries supported)
data Query :: forall k. k -> Type
data Query a

-- | Component actions
data Action
  = Initialize                            -- Set up JS drag handler
  | ReceiveAtoms (Array String)
  | SelectTimePoint ASP.TimePoint
  | ClickTimelineEvent ASP.TimelineEvent  -- User clicked on a specific event
  | ToggleViewMode                        -- Switch between SVG and HTML views
  | ToggleBagPanel                        -- Toggle bag panel collapsed state
  | ToggleScriptPanel                     -- Toggle script panel collapsed state
  -- Mouse events for SVG view (HTML view uses JS pointer events)
  | StartDragReminderMouse { reminder :: { token :: String, player :: String, placedAt :: ASP.TimePoint }, event :: MouseEvent }
  | DragMoveMouse MouseEvent
  | EndDragMouse MouseEvent
  | CancelDrag
  -- Drop event from JS drag handler (for HTML view)
  | HandleReminderDrop Drag.DropEvent
  -- Drop event from JS role drag handler (for HTML view)
  | HandleRoleDrop RoleDrag.DropEvent

-- | The Halogen component with output events
component :: forall m. MonadEffect m => H.Component Query (Array String) Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< ReceiveAtoms
        , initialize = Just Initialize
        }
    }

-- | Initial state from input
initialState :: Array String -> State
initialState atomStrings =
  let
    parsedAtoms = ASP.parseAnswerSetWithOriginals atomStrings
    atoms = map _.atom parsedAtoms
    timeline = ASP.extractTimelineWithSources parsedAtoms
    allTimes = getAllTimePoints atoms
    firstTime = head allTimes
  in
    { parsedAtoms
    , atoms
    , timeline
    , selectedTime: firstTime
    , allTimePoints: allTimes
    , dragging: Nothing
    , svgBounds: Nothing
    , viewMode: HtmlView  -- Default to HTML view for better mobile support
    , bagCollapsed: false    -- Bag panel starts expanded
    , scriptCollapsed: false -- Script panel starts expanded
    }

-- | Get all unique time points from atoms
-- | Only includes time points from event predicates and structural predicates (time, acting_role).
-- | Excludes state predicates (alive, dead, reminder_on) since those represent ongoing state.
getAllTimePoints :: Array ASP.Atom -> Array ASP.TimePoint
getAllTimePoints atoms =
  nub $ sortBy ASP.compareTimePoints $ Array.mapMaybe getTimeFromAtom atoms
  where
    getTimeFromAtom atom =
      case ASP.atomCategory atom of
        -- Include events and structural predicates
        ASP.EventPredicate -> getTimePointFromAtom atom
        ASP.StructuralPredicate -> getTimePointFromAtom atom
        -- Exclude state predicates (alive, dead, ghost_vote_used, reminder_on)
        -- These represent ongoing state, not discrete events
        ASP.StatePredicate -> Nothing
        ASP.OtherPredicate -> Nothing

    getTimePointFromAtom (ASP.Time t) = Just t
    getTimePointFromAtom (ASP.StTells _ _ _ t) = Just t
    getTimePointFromAtom (ASP.PlayerChooses _ _ _ t) = Just t
    getTimePointFromAtom (ASP.ActingRole t _) = Just t
    getTimePointFromAtom (ASP.Executed _ d) = Just (ASP.Day d "exec")  -- Executions happen during day
    getTimePointFromAtom _ = Nothing

-- | Find the original source string for an atom matching a given time point
-- | Preference order:
-- | 1. time(T) - explicit time marker
-- | 2. Event predicates at time T (d_st_tells, d_player_chooses)
-- | 3. acting_role(T, _) - structural but time-specific
-- | This ensures we never scroll to state predicates like alive/dead/reminder_on
findTimeAtomSource :: Array ASP.ParsedAtom -> ASP.TimePoint -> Maybe String
findTimeAtomSource parsedAtoms targetTime =
  -- First try: explicit time atom
  case Array.find isMatchingTimeAtom parsedAtoms of
    Just { original } | original /= "" -> Just original
    _ ->
      -- Second try: any event predicate at this time
      case Array.find isEventAtTime parsedAtoms of
        Just { original } | original /= "" -> Just original
        _ ->
          -- Third try: acting_role at this time
          case Array.find isActingRoleAtTime parsedAtoms of
            Just { original } | original /= "" -> Just original
            _ -> Nothing
  where
    isMatchingTimeAtom { atom: ASP.Time t } = t == targetTime
    isMatchingTimeAtom _ = false

    isEventAtTime { atom } =
      ASP.atomCategory atom == ASP.EventPredicate &&
      getTimeFromEventAtom atom == Just targetTime

    isActingRoleAtTime { atom: ASP.ActingRole t _ } = t == targetTime
    isActingRoleAtTime _ = false

    getTimeFromEventAtom (ASP.StTells _ _ _ t) = Just t
    getTimeFromEventAtom (ASP.PlayerChooses _ _ _ t) = Just t
    getTimeFromEventAtom _ = Nothing

-- | Main render function
render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  let
    gameState = case state.selectedTime of
      Just t -> ASP.buildGameState state.atoms t
      Nothing -> ASP.buildGameState state.atoms (ASP.Night 1 0 0)
  in
  HH.div
    [ HP.style "display: flex; gap: 20px; margin-top: 20px; flex-wrap: wrap;" ]
    [ -- Timeline panel (leftmost)
      HH.div
        [ HP.style "flex: 1; min-width: 300px; max-width: 400px;" ]
        [ HH.h3
            [ HP.style "margin: 0 0 10px 0; color: #333;" ]
            [ HH.text "Timeline" ]
        , renderTimeline state
        ]
    -- Grimoire area: Grimoire | Bag | Script
    , HH.div
        [ HP.style "flex: 2; min-width: 400px; display: flex; gap: 10px; align-items: stretch;" ]
        [ -- Grimoire panel (left)
          HH.div
            [ HP.style $ "flex: 1;"
                <> if isJust state.dragging then " cursor: grabbing;" else ""
            ]
            [ -- Header with toggle button
              HH.div
                [ HP.style "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;" ]
                [ HH.h3
                    [ HP.style "margin: 0; color: #333;" ]
                    [ HH.text $ "Grimoire" <> case state.selectedTime of
                        Just t -> " @ " <> formatTimePoint t
                        Nothing -> ""
                    ]
                , HH.button
                    [ HP.style $ "padding: 4px 10px; font-size: 11px; border-radius: 4px; "
                        <> "border: 1px solid #ccc; background: #f5f5f5; cursor: pointer;"
                    , HE.onClick \_ -> ToggleViewMode
                    ]
                    [ HH.text $ if state.viewMode == SvgView then "Grid View" else "Circle View" ]
                ]
            , HH.p
                [ HP.style "font-size: 11px; color: #888; margin: 0 0 5px 0; font-style: italic;" ]
                [ HH.text $ "Drag role or reminder tokens to move them between players"
                    <> if state.viewMode == HtmlView then " (touch supported)" else ""
                ]
            , if state.viewMode == SvgView
                then renderGrimoire state
                else renderHtmlGrimoire state
            ]
        -- Bag panel (center, collapsible)
        , renderBagPanel state gameState
        -- Script panel (right, collapsible)
        , renderScriptPanel state
        ]
    -- Debug panel for reminder tokens (collapsible)
    , renderReminderDebugPanel state gameState
    ]

-- | Render the bag panel (collapsible, shows tokens in the physical bag)
renderBagPanel :: forall cs m.
  State ->
  { players :: Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean }
  , reminders :: Array { token :: String, player :: String, placedAt :: ASP.TimePoint }
  , time :: ASP.TimePoint
  , bagTokens :: Array String
  , assignedNotInBag :: Array String
  } ->
  H.ComponentHTML Action cs m
renderBagPanel state gameState =
  let
    timeStr = case state.selectedTime of
      Just t -> formatTimePoint t
      Nothing -> ""
  in
  HH.div
    [ HP.style $ "min-width: " <> (if state.bagCollapsed then "40px" else "150px") <> "; "
        <> "display: flex; flex-direction: column; "
        <> "overflow: hidden; "  -- Prevent content from pushing height beyond grimoire
        <> "transition: min-width 0.2s ease-in-out;"
    ]
    [ -- Header with collapse toggle
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 10px;" ]
        [ HH.button
            [ HP.style $ "padding: 4px 8px; font-size: 11px; border-radius: 4px; "
                <> "border: 1px solid #ccc; background: #f5f5f5; cursor: pointer; "
                <> "min-width: 24px;"
            , HE.onClick \_ -> ToggleBagPanel
            , HP.title $ if state.bagCollapsed then "Expand bag" else "Collapse bag"
            ]
            [ HH.text $ if state.bagCollapsed then ">" else "<" ]
        , if state.bagCollapsed
            then HH.text ""
            else HH.h3
              [ HP.style "margin: 0; color: #333; font-size: 14px;" ]
              [ HH.text "Bag" ]
        ]
    -- Bag content (hidden when collapsed)
    , if state.bagCollapsed
        then HH.text ""
        else HH.div
          [ HP.style $ "flex: 1; min-height: 0; overflow-y: auto; "
              <> "border: 1px solid #ddd; border-radius: 4px; "
              <> "background: white; padding: 8px 20px 8px 8px;"  -- Extra right padding for scroll touch area
          -- Make bag container a drop target
          , HP.attr (HH.AttrName "data-player") "__bag__"
          ]
          [ if null gameState.bagTokens
              then HH.p
                [ HP.style "color: #666; font-style: italic; font-size: 12px;" ]
                [ HH.text "Bag empty" ]
              else HH.div
                [ HP.style "display: flex; flex-wrap: wrap; gap: 6px;" ]
                (map (renderBagToken timeStr) gameState.bagTokens)
          ]
    -- Assigned but not in bag section (e.g., Drunk)
    , if state.bagCollapsed || null gameState.assignedNotInBag
        then HH.text ""
        else HH.div
          [ HP.style "margin-top: 12px;" ]
          [ HH.h4
              [ HP.style "margin: 0 0 8px 0; color: #666; font-size: 12px; font-weight: normal;" ]
              [ HH.text "Assigned (not in bag)" ]
          , HH.div
              [ HP.style $ "border: 1px dashed #ccc; border-radius: 4px; "
                  <> "background: #f9f9f9; padding: 8px;"
              ]
              [ HH.div
                  [ HP.style "display: flex; flex-wrap: wrap; gap: 6px;" ]
                  (map (renderAssignedNotInBagToken timeStr) gameState.assignedNotInBag)
              ]
          ]
    ]

-- | Render a token for an assigned-but-not-in-bag role (like Drunk)
-- | These are displayed with a dashed border to distinguish from bag tokens
renderAssignedNotInBagToken :: forall cs m.
  String ->  -- time string
  String ->  -- role name
  H.ComponentHTML Action cs m
renderAssignedNotInBagToken timeStr role =
  let
    roleColor = getRoleColor role
  in
  HH.div
    [ HP.style $ "width: 48px; height: 48px; border-radius: 50%; flex-shrink: 0; "
        <> "background: " <> roleColor <> "; "
        <> "border: 2px dashed white; box-shadow: 0 1px 3px rgba(0,0,0,0.2); "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "font-size: 8px; font-weight: bold; color: white; text-align: center; padding: 2px; "
        <> "opacity: 0.85;"  -- Slightly faded to indicate special status
    , HP.attr (HH.AttrName "data-role-token") role
    , HP.attr (HH.AttrName "data-role-player") "__assigned__"
    , HP.attr (HH.AttrName "data-role-time") timeStr
    , HP.attr (HH.AttrName "data-role-color") roleColor
    , HP.attr (HH.AttrName "data-role-display") (formatRoleName role)
    , HP.title $ (formatRoleName role) <> " (assigned, not received)"
    ]
    [ HH.text $ formatRoleName role ]

-- | Render a single token in the bag (just the role circle, draggable)
renderBagToken :: forall cs m.
  String ->  -- time string
  String ->  -- role name
  H.ComponentHTML Action cs m
renderBagToken timeStr role =
  let
    roleColor = getRoleColor role
  in
  HH.div
    [ HP.style $ "width: 48px; height: 48px; border-radius: 50%; flex-shrink: 0; "
        <> "background: " <> roleColor <> "; "
        <> "border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.2); "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "cursor: grab; touch-action: none; user-select: none; "
        <> "font-size: 8px; font-weight: bold; color: white; text-align: center; padding: 2px;"
    -- Data attributes for drag - uses __bag__ as source indicator
    , HP.attr (HH.AttrName "data-role-token") role
    , HP.attr (HH.AttrName "data-role-player") "__bag__"
    , HP.attr (HH.AttrName "data-role-time") timeStr
    , HP.attr (HH.AttrName "data-role-color") roleColor
    , HP.attr (HH.AttrName "data-role-display") (formatRoleName role)
    , HP.title (formatRoleName role)
    ]
    [ HH.text $ formatRoleName role ]

-- | Render the script panel (collapsible, shows all roles in the script)
renderScriptPanel :: forall cs m. State -> H.ComponentHTML Action cs m
renderScriptPanel state =
  let
    timeStr = case state.selectedTime of
      Just t -> formatTimePoint t
      Nothing -> ""
  in
  HH.div
    [ HP.style $ "min-width: " <> (if state.scriptCollapsed then "40px" else "160px") <> "; "
        <> "display: flex; flex-direction: column; "
        <> "overflow: hidden; "  -- Prevent content from pushing height beyond grimoire
        <> "transition: min-width 0.2s ease-in-out;"
    ]
    [ -- Header with collapse toggle
      HH.div
        [ HP.style "display: flex; align-items: center; gap: 8px; margin-bottom: 10px; justify-content: flex-end;" ]
        [ if state.scriptCollapsed
            then HH.text ""
            else HH.h3
              [ HP.style "margin: 0; color: #333; font-size: 14px;" ]
              [ HH.text "Script" ]
        , HH.button
            [ HP.style $ "padding: 4px 8px; font-size: 11px; border-radius: 4px; "
                <> "border: 1px solid #ccc; background: #f5f5f5; cursor: pointer; "
                <> "min-width: 24px;"
            , HE.onClick \_ -> ToggleScriptPanel
            , HP.title $ if state.scriptCollapsed then "Expand script" else "Collapse script"
            ]
            [ HH.text $ if state.scriptCollapsed then "<" else ">" ]
        ]
    -- Script content (hidden when collapsed)
    , if state.scriptCollapsed
        then HH.text ""
        else HH.div
          [ HP.style $ "flex: 1; min-height: 0; overflow-y: auto; "
              <> "border: 1px solid #ddd; border-radius: 4px; "
              <> "background: white; padding: 8px 8px 8px 20px;"  -- Extra left padding for scroll touch area
          ]
          [ -- Townsfolk section
            renderScriptSection "Townsfolk" tbTownsfolk timeStr
          -- Outsiders section
          , renderScriptSection "Outsiders" tbOutsiders timeStr
          -- Minions section
          , renderScriptSection "Minions" tbMinions timeStr
          -- Demons section
          , renderScriptSection "Demons" tbDemons timeStr
          ]
    ]

-- | Render a section of the script (category header + role tokens)
renderScriptSection :: forall cs m. String -> Array String -> String -> H.ComponentHTML Action cs m
renderScriptSection categoryName roles timeStr =
  HH.div
    [ HP.style "margin-bottom: 8px;" ]
    [ HH.div
        [ HP.style "font-size: 10px; font-weight: bold; color: #888; text-transform: uppercase; margin-bottom: 4px;" ]
        [ HH.text categoryName ]
    , HH.div
        [ HP.style "display: flex; flex-direction: column; gap: 4px;" ]
        (map (renderScriptRole timeStr) roles)
    ]

-- | Render a single role in the script (draggable)
renderScriptRole :: forall cs m. String -> String -> H.ComponentHTML Action cs m
renderScriptRole timeStr role =
  let
    roleColor = getRoleColor role
  in
  HH.div
    [ HP.style $ "display: flex; align-items: center; gap: 6px; padding: 3px; "
        <> "border-radius: 4px; background: #f9f9f9; cursor: grab; "
        <> "touch-action: none; user-select: none;"
    -- Data attributes for drag - uses __script__ as source indicator
    , HP.attr (HH.AttrName "data-role-token") role
    , HP.attr (HH.AttrName "data-role-player") "__script__"
    , HP.attr (HH.AttrName "data-role-time") timeStr
    , HP.attr (HH.AttrName "data-role-color") roleColor
    , HP.attr (HH.AttrName "data-role-display") (formatRoleName role)
    ]
    [ -- Small color indicator
      HH.div
        [ HP.style $ "width: 20px; height: 20px; border-radius: 50%; flex-shrink: 0; "
            <> "background: " <> roleColor <> "; "
            <> "border: 1px solid rgba(0,0,0,0.1);"
        ]
        []
    -- Role name
    , HH.span
        [ HP.style "font-size: 11px; color: #333;" ]
        [ HH.text $ formatRoleName role ]
    ]

-- | Render debug panel showing all reminder token data
renderReminderDebugPanel :: forall cs m.
  State ->
  { players :: Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean }
  , reminders :: Array { token :: String, player :: String, placedAt :: ASP.TimePoint }
  , time :: ASP.TimePoint
  , bagTokens :: Array String
  , assignedNotInBag :: Array String
  } ->
  H.ComponentHTML Action cs m
renderReminderDebugPanel state gameState =
  let
    -- Extract all ReminderOn atoms from parsed atoms
    allReminderAtoms = Array.mapMaybe getReminderOnAtom state.atoms

    getReminderOnAtom (ASP.ReminderOn token player time) =
      Just { token, player, time }
    getReminderOnAtom _ = Nothing

    targetTimeStr = case state.selectedTime of
      Just t -> formatTimePoint t
      Nothing -> "None (defaulting to Night 1 (0.0))"

    targetTime = fromMaybe (ASP.Night 1 0 0) state.selectedTime

    -- Group reminders by time for easier viewing
    remindersByTime = Array.sortBy (comparing _.time) allReminderAtoms

    -- Find reminders at the target time specifically
    remindersAtTarget = Array.filter (\r -> r.time == targetTime) allReminderAtoms
  in
  HH.div
    [ HP.style "margin-top: 20px; width: 100%;" ]
    [ HH.details
        [ HP.style "border: 1px solid #f0ad4e; border-radius: 4px; background: #fcf8e3; padding: 8px;" ]
        [ HH.summary
            [ HP.style "cursor: pointer; font-weight: bold; color: #8a6d3b;" ]
            [ HH.text $ "🔍 Reminder Debug (" <> show (Array.length gameState.reminders) <> " displayed, "
                <> show (Array.length allReminderAtoms) <> " total atoms)" ]
        , HH.div
            [ HP.style "margin-top: 10px; font-size: 12px; font-family: monospace;" ]
            [ HH.div
                [ HP.style "margin-bottom: 8px; padding: 8px; background: #fff; border-radius: 4px;" ]
                [ HH.strong_ [ HH.text "Selected Time: " ]
                , HH.text targetTimeStr
                ]
            , HH.div
                [ HP.style "margin-bottom: 8px; padding: 8px; background: #fff; border-radius: 4px;" ]
                [ HH.strong_ [ HH.text $ "Reminders at target time (" <> show (Array.length remindersAtTarget) <> "):" ]
                , if Array.null remindersAtTarget
                    then HH.div [ HP.style "color: #a94442; font-style: italic;" ] [ HH.text "None found - this explains why no tokens are displayed!" ]
                    else HH.ul [ HP.style "margin: 4px 0; padding-left: 20px;" ]
                      (map (\r -> HH.li_ [ HH.text $ r.token <> " on " <> r.player ]) remindersAtTarget)
                ]
            , HH.div
                [ HP.style "margin-bottom: 8px; padding: 8px; background: #fff; border-radius: 4px;" ]
                [ HH.strong_ [ HH.text $ "Displayed reminders (from gameState, " <> show (Array.length gameState.reminders) <> "):" ]
                , if Array.null gameState.reminders
                    then HH.div [ HP.style "color: #a94442; font-style: italic;" ] [ HH.text "None" ]
                    else HH.ul [ HP.style "margin: 4px 0; padding-left: 20px;" ]
                      (map (\r -> HH.li_ [ HH.text $ r.token <> " on " <> r.player <> " (placed @ " <> formatTimePoint r.placedAt <> ")" ]) gameState.reminders)
                ]
            , HH.div
                [ HP.style "padding: 8px; background: #fff; border-radius: 4px; max-height: 200px; overflow-y: auto;" ]
                [ HH.strong_ [ HH.text $ "All ReminderOn atoms parsed (" <> show (Array.length allReminderAtoms) <> "):" ]
                , if Array.null allReminderAtoms
                    then HH.div [ HP.style "color: #a94442; font-style: italic;" ] [ HH.text "No reminder_on atoms found in answer set!" ]
                    else HH.ul [ HP.style "margin: 4px 0; padding-left: 20px;" ]
                      (map (\r -> HH.li_ [ HH.text $ r.token <> " on " <> r.player <> " @ " <> formatTimePoint r.time ]) remindersByTime)
                ]
            ]
        ]
    ]

-- | Render the timeline with events
renderTimeline :: forall cs m. State -> H.ComponentHTML Action cs m
renderTimeline state =
  HH.div
    [ HP.style $ "max-height: 400px; overflow-y: auto; "
        <> "border: 1px solid #ddd; border-radius: 4px; "
        <> "background: white;"
    ]
    [ if null state.allTimePoints
        then HH.p
          [ HP.style "padding: 15px; color: #666; font-style: italic;" ]
          [ HH.text "No timeline data available. Add #show directives for time/1, d_st_tells/4, d_player_chooses/4, etc." ]
        else HH.div_ $ map (renderTimePoint state) state.allTimePoints
    ]

-- | Render a single time point in the timeline
renderTimePoint :: forall cs m. State -> ASP.TimePoint -> H.ComponentHTML Action cs m
renderTimePoint state timePoint =
  let
    isSelected = state.selectedTime == Just timePoint
    events = filter (eventAtTime timePoint) state.timeline
    bgColor = if isSelected then "#e3f2fd" else "white"
  in
    HH.div
      [ HP.style $ "border-bottom: 1px solid #eee; cursor: pointer; "
          <> "background: " <> bgColor <> ";"
      , HE.onClick \_ -> SelectTimePoint timePoint
      ]
      [ -- Time point header
        HH.div
          [ HP.style $ "padding: 8px 12px; font-weight: bold; "
              <> "color: " <> (if isSelected then "#1565c0" else "#333") <> ";"
              <> "font-size: 13px;"
          ]
          [ HH.text $ formatTimePoint timePoint ]
      -- Events at this time
      , if null events
          then HH.text ""
          else HH.div
            [ HP.style "padding: 0 12px 8px 24px;" ]
            (map renderEvent events)
      ]

-- | Check if an event occurs at a specific time
eventAtTime :: ASP.TimePoint -> ASP.TimelineEvent -> Boolean
eventAtTime t (ASP.RoleAction r) = r.time == t
eventAtTime t (ASP.TokenPlaced r) = r.time == t
eventAtTime t (ASP.Death d) = d.time == t
eventAtTime (ASP.Day d _) (ASP.Execution e) = e.day == d  -- Executions appear under their day
eventAtTime _ (ASP.Execution _) = false

-- | Render a single event (clickable for navigation)
renderEvent :: forall cs m. ASP.TimelineEvent -> H.ComponentHTML Action cs m
renderEvent event =
  case event of
    ASP.RoleAction r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #555; margin: 4px 0; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to highlight this atom in the answer set"
        ]
        [ HH.span
            [ HP.style $ "display: inline-block; padding: 2px 6px; border-radius: 3px; "
                <> "background: " <> (if r.eventType == "d_st_tells" then "#e8f5e9" else "#fff3e0") <> "; "
                <> "margin-right: 6px;"
            ]
            [ HH.text $ formatRoleName r.role ]
        , HH.text $ if r.eventType == "d_st_tells"
            then "tells " <> r.player <> ": " <> r.message
            else r.player <> " chooses: " <> r.message
        ]
    ASP.TokenPlaced r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #888; margin: 4px 0; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to highlight this atom in the answer set"
        ]
        [ HH.text $ r.token <> " placed on " <> r.player ]
    ASP.Execution r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #c62828; margin: 4px 0; font-weight: bold; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to highlight this atom in the answer set"
        ]
        [ HH.text $ r.player <> " executed" ]
    ASP.Death r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #7b1fa2; margin: 4px 0; font-weight: bold; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to highlight this atom in the answer set"
        ]
        [ HH.text $ r.player <> " died" ]

-- | Render the grimoire (players in a circle)
renderGrimoire :: forall cs m. State -> H.ComponentHTML Action cs m
renderGrimoire state =
  let
    gameState = case state.selectedTime of
      Just t -> ASP.buildGameState state.atoms t
      Nothing -> ASP.buildGameState state.atoms (ASP.Night 1 0 0)
    playerCount = length gameState.players
    -- SVG dimensions
    width = 500.0
    height = 500.0
    centerX = width / 2.0
    centerY = height / 2.0
    radius = 180.0
    -- Drag state
    isDragging = isJust state.dragging
  in
    HH.div
      [ HP.style "background: #f5f5f5; border-radius: 8px; padding: 20px;" ]
      [ -- SVG grimoire with drag event handlers
        SE.svg
          ( [ SA.viewBox 0.0 0.0 width height
            , SA.width width
            , SA.height height
            , HP.id "grimoire-svg"
            , HP.style $ if isDragging then "cursor: grabbing;" else ""
            ] <> (if isDragging
                  then [ HE.onMouseMove DragMoveMouse
                       , HE.onMouseUp EndDragMouse
                       , HE.onMouseLeave \_ -> CancelDrag
                       ]
                  else [])
          )
          (
            -- Draw dashed circle showing table edge
            [ SE.circle
                [ SA.cx centerX
                , SA.cy centerY
                , SA.r radius
                , SA.fill NoColor
                , SA.stroke (Named "#ccc")
                , SA.strokeWidth 1.0
                , SA.strokeDashArray "4,4"
                ]
            ]
            -- Draw players
            <> (if playerCount > 0
                then mapWithIndex (renderPlayer centerX centerY radius playerCount gameState.reminders state.dragging) gameState.players
                else [ SE.text
                         [ SA.x centerX
                         , SA.y centerY
                         , SA.textAnchor AnchorMiddle
                         , SA.fill (Named "#999")
                         ]
                         [ HH.text "No player data - add #show chair/2." ]
                     ])
            -- Draw drag visual feedback
            <> renderDragFeedback state.dragging centerX centerY radius playerCount gameState.players
          )
      -- Legend
      , HH.div
          [ HP.style "margin-top: 15px; font-size: 12px; color: #666; text-align: center;" ]
          [ -- Character type color families (each role has unique color within family)
            HH.span [ HP.style "margin-right: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 36px; height: 12px; border-radius: 6px; background: linear-gradient(90deg, #0D47A1, #2196F3, #00BCD4, #673AB7, #689F38); margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Townsfolk"
              ]
          , HH.span [ HP.style "margin-right: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 24px; height: 12px; border-radius: 6px; background: linear-gradient(90deg, #26A69A, #4DD0E1, #78909C); margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Outsider"
              ]
          , HH.span [ HP.style "margin-right: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 24px; height: 12px; border-radius: 6px; background: linear-gradient(90deg, #E65100, #FF9800, #E53935, #8D6E63); margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Minion"
              ]
          , HH.span [ HP.style "margin-right: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #B71C1C; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Demon"
              ]
          -- Status indicators
          , HH.span [ HP.style "margin-left: 10px; margin-right: 12px; border-left: 1px solid #ccc; padding-left: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 3px solid #4CAF50; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Alive"
              ]
          , HH.span [ HP.style "margin-right: 12px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 3px solid #9e9e9e; position: relative; margin-right: 4px; vertical-align: middle;" ]
                  [ HH.span [ HP.style "position: absolute; width: 14px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(45deg);" ] [] ]
              , HH.text "Dead"
              ]
          , HH.span_
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 3px solid #9e9e9e; position: relative; margin-right: 4px; vertical-align: middle;" ]
                  [ HH.span [ HP.style "position: absolute; width: 14px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(45deg);" ] []
                  , HH.span [ HP.style "position: absolute; width: 14px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(-45deg);" ] []
                  ]
              , HH.text "No Vote"
              ]
          ]
      ]

-- | Calculate player position on the circle
calcPlayerPosition :: forall r.
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- total players
  Int ->     -- index
  { name :: String | r } ->
  { name :: String, x :: Number, y :: Number }
calcPlayerPosition centerX centerY radius playerCount idx player =
  let
    angle = (toNumber idx) * 2.0 * pi / (toNumber playerCount) - pi / 2.0
    x = centerX + radius * cos angle
    y = centerY + radius * sin angle
  in
    { name: player.name, x, y }

-- | Render a single player token
renderPlayer :: forall cs m.
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- total players
  Array { token :: String, player :: String, placedAt :: ASP.TimePoint } ->  -- reminders (sorted by placedAt)
  Maybe DragState ->  -- current drag state
  Int ->     -- index
  { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean } ->
  H.ComponentHTML Action cs m
renderPlayer centerX centerY radius playerCount reminders dragState idx player =
  let
    -- Calculate position on circle (start from top, go clockwise)
    angle = (toNumber idx) * 2.0 * pi / (toNumber playerCount) - pi / 2.0
    x = centerX + radius * cos angle
    y = centerY + radius * sin angle

    -- Player's reminders (filter out the one being dragged)
    playerReminders = filter (\r -> r.player == player.name && not (isDraggingReminder dragState r)) reminders

    -- Direction toward center (angle + pi points inward)
    angleToCenter = angle + pi

    -- Colors
    aliveColor = if player.alive then "#4CAF50" else "#9e9e9e"
    roleColor = getRoleColor player.role

    -- Is this player the drop target?
    isDropTarget = case dragState of
      Just ds -> ds.targetPlayer == Just player.name
      Nothing -> false
  in
    SE.g
      [ SA.transform [ SA.Translate x y ] ]
      ( [
        -- Main role token (outer circle) - highlight if drop target
        SE.circle
          [ SA.cx 0.0
          , SA.cy 0.0
          , SA.r 35.0
          , SA.fill (Named roleColor)
          , SA.stroke (Named $ if isDropTarget then "#FFD700" else aliveColor)
          , SA.strokeWidth (if isDropTarget then 5.0 else 3.0)
          ]
      -- Drop target indicator (dashed inner circle)
      ] <> (if isDropTarget
            then [ SE.circle
                     [ SA.cx 0.0
                     , SA.cy 0.0
                     , SA.r 42.0
                     , SA.fill NoColor
                     , SA.stroke (Named "#FFD700")
                     , SA.strokeWidth 2.0
                     , SA.strokeDashArray "4,4"
                     ]
                 ]
            else [])
      -- Death slash (downward \ from top-right to bottom-left)
      <> (if not player.alive
            then [ SE.line
                     [ SA.x1 20.0
                     , SA.y1 (-20.0)
                     , SA.x2 (-20.0)
                     , SA.y2 20.0
                     , SA.stroke (Named "#1a1a1a")
                     , SA.strokeWidth 4.0
                     ]
                 ]
            else [])
      -- Ghost vote used slash (upward / from top-left to bottom-right, forming X with death slash)
      <> (if not player.alive && player.ghostVoteUsed
            then [ SE.line
                     [ SA.x1 (-20.0)
                     , SA.y1 (-20.0)
                     , SA.x2 20.0
                     , SA.y2 20.0
                     , SA.stroke (Named "#1a1a1a")
                     , SA.strokeWidth 4.0
                     ]
                 ]
            else [])
      <> [
      -- Player name
        SE.text
          [ SA.x 0.0
          , SA.y (-8.0)
          , SA.textAnchor AnchorMiddle
          , SA.fill (Named "white")
          , SA.fontWeight FWeightBold
          , SA.fontSize (FontSizeLength (Px 10.0))
          ]
          [ HH.text player.name ]
      -- Role name
      , SE.text
          [ SA.x 0.0
          , SA.y 5.0
          , SA.textAnchor AnchorMiddle
          , SA.fill (Named "white")
          , SA.fontSize (FontSizeLength (Px 8.0))
          ]
          [ HH.text $ formatRoleName player.role ]
      -- Token (what they think they are, if different)
      , if player.token /= player.role
          then SE.text
            [ SA.x 0.0
            , SA.y 16.0
            , SA.textAnchor AnchorMiddle
            , SA.fill (Named "#ffeb3b")
            , SA.fontSize (FontSizeLength (Px 7.0))
            ]
            [ HH.text $ "(" <> formatRoleName player.token <> ")" ]
          else SE.g [] []
      -- Reminder tokens positioned toward center
      ] <> mapWithIndex (renderReminderToken angleToCenter (length playerReminders)) playerReminders
      )

-- | Check if a reminder is currently being dragged
isDraggingReminder :: Maybe DragState -> { token :: String, player :: String, placedAt :: ASP.TimePoint } -> Boolean
isDraggingReminder Nothing _ = false
isDraggingReminder (Just ds) r =
  ds.reminder.token == r.token && ds.reminder.player == r.player && ds.reminder.placedAt == r.placedAt

-- | Render a reminder token (draggable)
renderReminderToken :: forall cs m.
  Number -> -- angle toward center
  Int ->    -- total reminders for this player
  Int ->    -- index (0 = oldest/closest to role token)
  { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  H.ComponentHTML Action cs m
renderReminderToken angleToCenter _total idx reminder =
  let
    -- Position reminder tokens along line toward center
    -- Start just inside the player token (radius 35) and go inward
    baseDistance = 50.0  -- Starting distance from player center
    spacing = 28.0       -- Space between reminder tokens
    dist = baseDistance + (toNumber idx) * spacing
    rx = dist * cos angleToCenter
    ry = dist * sin angleToCenter
    isPseudo = isPseudoReminder reminder.token
    -- Shape element: circle for regular reminders, square for pseudo reminders
    shapeElement = if isPseudo
      then SE.rect
          [ SA.x (-10.0)
          , SA.y (-10.0)
          , SA.width 20.0
          , SA.height 20.0
          , SA.fill (Named (getReminderColor reminder.token))
          , SA.stroke (Named "#fff")
          , SA.strokeWidth 1.0
          , HP.style "cursor: grab;"
          , HE.onMouseDown \evt -> StartDragReminderMouse { reminder, event: evt }
          ]
      else SE.circle
          [ SA.cx 0.0
          , SA.cy 0.0
          , SA.r 12.0
          , SA.fill (Named (getReminderColor reminder.token))
          , SA.stroke (Named "#fff")
          , SA.strokeWidth 1.0
          , HP.style "cursor: grab;"
          , HE.onMouseDown \evt -> StartDragReminderMouse { reminder, event: evt }
          ]
  in
    SE.g
      [ SA.transform [ SA.Translate rx ry ] ]
      [ -- Reminder shape - attach drag handler here (shapes have geometry, receive events)
        shapeElement
      -- Reminder abbreviation - let clicks/touches pass through to shape
      , SE.text
          [ SA.x 0.0
          , SA.y 3.0
          , SA.textAnchor AnchorMiddle
          , SA.fill (Named "white")
          , SA.fontWeight FWeightBold
          , SA.fontSize (FontSizeLength (Px 6.0))
          , HP.style "cursor: grab; pointer-events: none;"
          ]
          [ HH.text $ abbreviateToken reminder.token ]
      ]

-- | Render visual feedback for dragging (dotted line + floating token)
renderDragFeedback :: forall cs m.
  Maybe DragState ->
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- playerCount
  Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean } ->
  Array (H.ComponentHTML Action cs m)
renderDragFeedback Nothing _ _ _ _ _ = []
renderDragFeedback (Just ds) centerX centerY radius playerCount players =
  let
    -- Calculate start position (where the token was originally)
    sourcePlayerPos = findPlayerPosition ds.reminder.player centerX centerY radius playerCount players
    -- Draw from source to current mouse position
  in case sourcePlayerPos of
    Nothing -> []
    Just { x: srcX, y: srcY } ->
      [ -- Dotted line from source to current position
        SE.line
          [ SA.x1 srcX
          , SA.y1 srcY
          , SA.x2 ds.currentX
          , SA.y2 ds.currentY
          , SA.stroke (Named "#666")
          , SA.strokeWidth 2.0
          , SA.strokeDashArray "5,5"
          ]
      -- Floating token at current mouse position
      , SE.g
          [ SA.transform [ SA.Translate ds.currentX ds.currentY ] ]
          [ SE.circle
              [ SA.cx 0.0
              , SA.cy 0.0
              , SA.r 14.0
              , SA.fill (Named (getReminderColor ds.reminder.token))
              , SA.stroke (Named "#FFD700")
              , SA.strokeWidth 2.0
              ]
          , SE.text
              [ SA.x 0.0
              , SA.y 4.0
              , SA.textAnchor AnchorMiddle
              , SA.fill (Named "white")
              , SA.fontWeight FWeightBold
              , SA.fontSize (FontSizeLength (Px 7.0))
              ]
              [ HH.text $ abbreviateToken ds.reminder.token ]
          ]
      ]

-- | Render HTML-based grimoire with hollow rectangle layout (players on perimeter, empty center)
renderHtmlGrimoire :: forall cs m. State -> H.ComponentHTML Action cs m
renderHtmlGrimoire state =
  let
    gameState = case state.selectedTime of
      Just t -> ASP.buildGameState state.atoms t
      Nothing -> ASP.buildGameState state.atoms (ASP.Night 1 0 0)
    playerCount = length gameState.players
    -- Calculate grid dimensions for hollow rectangle
    -- For n players, we want a grid where perimeter = n
    -- Perimeter of a×b grid = 2a + 2b - 4 (corners counted once)
    -- We aim for roughly square grids
    gridDims = calculateGridDimensions playerCount
    cols = gridDims.cols
    rows = gridDims.rows
    -- Assign players to perimeter positions (clockwise from top-left)
    -- Using debug version to diagnose position generation
    positionResult = assignPerimeterPositionsDebug playerCount cols rows
    playerPositions = positionResult.positions
    -- Debug info for diagnosing grid issues
    positionCount = length playerPositions
    playerNames = map _.name gameState.players
    debugInfo = "Grid debug: " <> show playerCount <> " players, "
             <> show cols <> "x" <> show rows <> " grid, "
             <> show positionCount <> " positions"
  in
    HH.div
      [ HP.style "background: #f5f5f5; border-radius: 8px; padding: 15px;" ]
      [ -- Debug panel (temporary - can be removed once issue is fixed)
        HH.div
          [ HP.style "font-size: 10px; color: #666; margin-bottom: 8px; padding: 4px; background: #e0e0e0; border-radius: 4px;" ]
          [ HH.text debugInfo
          , HH.text $ " | Players: " <> intercalate ", " playerNames
          ]
      , HH.div
          [ HP.style "font-size: 9px; color: #333; margin-bottom: 8px; padding: 4px; background: #ffe0e0; border-radius: 4px; word-break: break-all;" ]
          [ HH.text $ "Position calc: " <> positionResult.debug ]
      , -- Player grid with hollow center
        -- Note: JS handles all drag events via pointer events on document
        HH.div
          [ HP.style $ "display: grid; grid-template-columns: repeat(" <> show cols <> ", minmax(100px, 1fr)); "
              <> "gap: 8px; min-height: 200px;"
          ]
          ( if playerCount > 0
              then renderHollowGrid gameState.reminders state.selectedTime gameState.players playerPositions cols rows
              else [ HH.div
                       [ HP.style "grid-column: 1 / -1; text-align: center; color: #999; padding: 40px;" ]
                       [ HH.text "No player data - add #show chair/2." ]
                   ]
          )
      -- Compact legend
      , HH.div
          [ HP.style "margin-top: 12px; font-size: 11px; color: #666; text-align: center;" ]
          [ HH.span [ HP.style "margin-right: 10px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; border: 2px solid #4CAF50; margin-right: 3px; vertical-align: middle;" ] []
              , HH.text "Alive"
              ]
          , HH.span [ HP.style "margin-right: 10px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; border: 2px solid #9e9e9e; position: relative; margin-right: 3px; vertical-align: middle;" ]
                  [ HH.span [ HP.style "position: absolute; width: 10px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(45deg);" ] [] ]
              , HH.text "Dead"
              ]
          , HH.span_
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; border: 2px solid #9e9e9e; position: relative; margin-right: 3px; vertical-align: middle;" ]
                  [ HH.span [ HP.style "position: absolute; width: 10px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(45deg);" ] []
                  , HH.span [ HP.style "position: absolute; width: 10px; height: 2px; background: #1a1a1a; top: 50%; left: 50%; transform: translate(-50%, -50%) rotate(-45deg);" ] []
                  ]
              , HH.text "No Vote"
              ]
          ]
      ]

-- | Render a single player card in HTML grid view
renderHtmlPlayer :: forall cs m.
  Array { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  Maybe ASP.TimePoint ->  -- selected time for data attributes
  { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean } ->
  H.ComponentHTML Action cs m
renderHtmlPlayer reminders selectedTime player =
  let
    playerReminders = filter (\r -> r.player == player.name) reminders
    aliveColor = if player.alive then "#4CAF50" else "#9e9e9e"
    roleColor = getRoleColor player.role
    timeStr = case selectedTime of
      Just t -> formatTimePoint t
      Nothing -> ""
    -- Death/ghost vote slashes using CSS pseudo-element style gradient lines
    deathSlashStyle = if not player.alive
      then "position: relative; "
      else ""
  in
    HH.div
      [ HP.style $ "background: " <> roleColor <> "; border-radius: 8px; padding: 10px; "
          <> "border: 3px solid " <> aliveColor <> "; "
          <> "text-align: center; min-height: 80px; "
          <> deathSlashStyle
      , HP.attr (HH.AttrName "data-player") player.name
      ]
      (
      -- Death/ghost vote overlay slashes (rendered as absolute positioned div)
      (if not player.alive
        then [ HH.div
                 [ HP.style $ "position: absolute; top: 0; left: 0; right: 0; bottom: 0; "
                     <> "pointer-events: none; overflow: hidden; border-radius: 5px;"
                 ]
                 ([ -- Downward slash (\)
                    HH.div
                      [ HP.style $ "position: absolute; top: 50%; left: 50%; "
                          <> "width: 120%; height: 4px; "
                          <> "background: #1a1a1a; "
                          <> "transform: translate(-50%, -50%) rotate(45deg); "
                          <> "border-radius: 2px;"
                      ]
                      []
                  ] <>
                  -- Ghost vote used slash (/) - forms X with death slash
                  (if player.ghostVoteUsed
                    then [ HH.div
                             [ HP.style $ "position: absolute; top: 50%; left: 50%; "
                                 <> "width: 120%; height: 4px; "
                                 <> "background: #1a1a1a; "
                                 <> "transform: translate(-50%, -50%) rotate(-45deg); "
                                 <> "border-radius: 2px;"
                             ]
                             []
                         ]
                    else [])
                 )
             ]
        else [])
      <>
      [ -- Player name
        HH.div
          [ HP.style "color: white; font-weight: bold; font-size: 12px; margin-bottom: 2px;" ]
          [ HH.text player.name ]
      -- Role token (draggable via JS pointer events)
      , HH.div
          [ HP.style $ "color: rgba(255,255,255,0.9); font-size: 10px; "
              <> "cursor: grab; touch-action: none; user-select: none; "
              <> "padding: 4px 8px; border-radius: 4px; "
              <> "background: rgba(0,0,0,0.2); display: inline-block;"
          -- Data attributes for JS role drag handler
          -- Use player.token (received) not player.role (assigned) so Drunk's token is draggable
          , HP.attr (HH.AttrName "data-role-token") player.token
          , HP.attr (HH.AttrName "data-role-player") player.name
          , HP.attr (HH.AttrName "data-role-time") timeStr
          , HP.attr (HH.AttrName "data-role-color") (getRoleColor player.token)
          , HP.attr (HH.AttrName "data-role-display") (formatRoleName player.token)
          ]
          [ HH.text $ formatRoleName player.role ]
      -- Token (if different from role, shows what they think they are)
      , if player.token /= player.role
          then HH.div
            [ HP.style "color: #ffeb3b; font-size: 9px; margin-top: 2px;" ]
            [ HH.text $ "(" <> formatRoleName player.token <> ")" ]
          else HH.text ""
      -- Reminder tokens (draggable via JS pointer events)
      , if null playerReminders
          then HH.text ""
          else HH.div
            [ HP.style "display: flex; flex-wrap: wrap; gap: 4px; justify-content: center; margin-top: 6px;" ]
            (map (renderHtmlReminderToken selectedTime) playerReminders)
      ])

-- | Render a single reminder token in HTML view (draggable via JS pointer events)
renderHtmlReminderToken :: forall cs m.
  Maybe ASP.TimePoint ->  -- selected time for data attribute
  { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  H.ComponentHTML Action cs m
renderHtmlReminderToken selectedTime reminder =
  let
    timeStr = case selectedTime of
      Just t -> formatTimePoint t
      Nothing -> ""
    isPseudo = isPseudoReminder reminder.token
    -- Square (2px radius) for pseudo reminders, circle (50%) for regular
    borderRadius = if isPseudo then "2px" else "50%"
  in
    HH.div
      [ HP.style $ "width: 24px; height: 24px; border-radius: " <> borderRadius <> "; "
          <> "background: " <> getReminderColor reminder.token <> "; "
          <> "border: 1px solid white; display: flex; align-items: center; justify-content: center; "
          <> "cursor: grab; font-size: 7px; font-weight: bold; color: white; "
          <> "touch-action: none; user-select: none;"
      -- Data attributes for JS drag handler
      , HP.attr (HH.AttrName "data-reminder-token") reminder.token
      , HP.attr (HH.AttrName "data-reminder-player") reminder.player
      , HP.attr (HH.AttrName "data-reminder-time") timeStr
      , HP.attr (HH.AttrName "data-reminder-color") (getReminderColor reminder.token)
      , HP.attr (HH.AttrName "data-reminder-abbrev") (abbreviateToken reminder.token)
      ]
    [ HH.text $ abbreviateToken reminder.token ]

-- | Find a player's position on the circle
findPlayerPosition :: forall r.
  String ->  -- player name
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- playerCount
  Array { name :: String | r } ->
  Maybe { x :: Number, y :: Number }
findPlayerPosition playerName centerX centerY radius playerCount players =
  case findIndex (\p -> p.name == playerName) players of
    Nothing -> Nothing
    Just idx ->
      let
        angle = (toNumber idx) * 2.0 * pi / (toNumber playerCount) - pi / 2.0
        x = centerX + radius * cos angle
        y = centerY + radius * sin angle
      in Just { x, y }

-- | Simple hash function for strings
-- Returns a positive integer derived from the string
hashString :: String -> Int
hashString s =
  let chars = toCharArray s
      codes = map toCharCode chars
      -- Polynomial hash with prime multiplier, keep positive with mod
      rawHash = foldl (\acc c -> (acc * 31 + c) `mod` 1000000) 0 codes
  in if rawHash < 0 then rawHash + 1000000 else rawHash

-- | Convert hash to HSL color string
-- isEvil: true for minions/demons (no blue), false for good (no red)
hashToHslColor :: Boolean -> Int -> String
hashToHslColor isEvil h =
  let -- For good: hue 90-270 (green through blue, avoiding red/orange)
      -- For evil: hue 0-60 or 300-360 (red/orange/yellow/magenta, avoiding blue)
      hue = if isEvil
              then
                -- Evil: use 0-60 (red-yellow) or 300-360 (magenta-red)
                -- Map to 120 degree range split across two zones
                let hMod = h `mod` 120
                in if hMod < 60 then hMod else hMod + 240  -- 0-60 or 300-360
              else
                -- Good: use 90-270 (green through blue through purple)
                (h `mod` 180) + 90
      -- Saturation 55-75% for vivid but not garish colors
      sat = 55 + ((h / 7) `mod` 21)
      -- Lightness 40-55% for good visibility on dark and light backgrounds
      light = 40 + ((h / 13) `mod` 16)
  in "hsl(" <> show hue <> ", " <> show sat <> "%, " <> show light <> "%)"

-- | Get unique color for each role
-- Color is deterministically derived from role name hash
-- Good (Townsfolk, Outsiders): green/cyan/blue/purple tones (no red)
-- Evil (Minions, Demons): red/orange/yellow/magenta tones (no blue)
getRoleColor :: String -> String
getRoleColor role =
  let h = hashString role
      isEvil = isMinion role || isDemon role
  in hashToHslColor isEvil h

isMinion :: String -> Boolean
isMinion r = r == "poisoner" || r == "spy" || r == "scarlet_woman" || r == "baron"

isDemon :: String -> Boolean
isDemon r = r == "imp"

-- | Check if a reminder token is a pseudo-reminder (not owned by any role)
-- Pseudo reminders include execution day markers (ex_d1, ex_d2, etc.)
isPseudoReminder :: String -> Boolean
isPseudoReminder token = S.take 3 token == "ex_"

-- | Map token prefix to the role it belongs to
tokenToRole :: String -> String
tokenToRole token
  | S.take 4 token == "poi_" = "poisoner"
  | S.take 4 token == "imp_" = "imp"
  | S.take 5 token == "monk_" = "monk"
  | S.take 3 token == "ft_" = "fortune_teller"
  | S.take 3 token == "ww_" = "washerwoman"
  | S.take 4 token == "lib_" = "librarian"
  | S.take 4 token == "inv_" = "investigator"
  | S.take 4 token == "but_" = "butler"
  | S.take 4 token == "sla_" = "slayer"
  | S.take 4 token == "emp_" = "empath"
  | S.take 4 token == "spy_" = "spy"
  | S.take 3 token == "sw_" = "scarlet_woman"
  | S.take 4 token == "rav_" = "ravenkeeper"
  | S.take 5 token == "chef_" = "chef"
  | S.take 4 token == "und_" = "undertaker"
  | S.take 4 token == "vir_" = "virgin"
  | S.take 4 token == "sol_" = "soldier"
  | S.take 4 token == "may_" = "mayor"
  | otherwise = "unknown"

-- | Get color for reminder token (same color as the source role)
getReminderColor :: String -> String
getReminderColor token = getRoleColor (tokenToRole token)

-- | Abbreviate token name for display
abbreviateToken :: String -> String
abbreviateToken t = case t of
  "poi_poisoned" -> "POI"
  "imp_dead" -> "DIE"
  "monk_protected" -> "PRO"
  "ft_red_herring" -> "RH"
  "ww_townsfolk" -> "WW"
  "ww_wrong" -> "WW?"
  "lib_outsider" -> "LIB"
  "lib_wrong" -> "LIB?"
  "inv_minion" -> "INV"
  "inv_wrong" -> "INV?"
  "but_master" -> "BUT"
  "sla_no_ability" -> "SLA"
  "drunk_is_drunk" -> "DRUNK"
  -- Execution day tokens
  "ex_d1" -> "EX1"
  "ex_d2" -> "EX2"
  "ex_d3" -> "EX3"
  "ex_d4" -> "EX4"
  "ex_d5" -> "EX5"
  _ -> takeChars 3 t
  where
    takeChars n s = if S.length s <= n then s else fold (take n (S.split (Pattern "") s))

-- | Format role name for display (capitalize, replace underscores)
formatRoleName :: String -> String
formatRoleName role =
  let
    parts = S.split (Pattern "_") role
    capitalized = map capitalize parts
  in
    intercalate " " capitalized
  where
    capitalize s = case Array.uncons (S.split (Pattern "") s) of
      Just { head: h, tail: t } -> S.toUpper h <> fold t
      Nothing -> s

-- | Format time point for display
formatTimePoint :: ASP.TimePoint -> String
formatTimePoint ASP.Setup = "Setup"
formatTimePoint (ASP.Night n 0 0) = "Night " <> show n <> " (Setup)"
formatTimePoint (ASP.Night n r s) = "Night " <> show n <> " (" <> show r <> "." <> show s <> ")"
formatTimePoint (ASP.Dawn n) = "Dawn " <> show n
formatTimePoint (ASP.Day n "0") = "Day " <> show n <> " (Start)"
formatTimePoint (ASP.Day n "exec") = "Day " <> show n <> " (Execution)"
formatTimePoint (ASP.Day n p) = "Day " <> show n <> " (" <> p <> ")"
formatTimePoint (ASP.UnknownTime s) = s

-- | Handle actions
handleAction :: forall cs m. MonadEffect m => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
  Initialize -> do
    -- Set up JS drag handlers for HTML view
    liftEffect Drag.initDragHandler
    liftEffect RoleDrag.initDragHandler
    -- Subscribe to reminder drop events from JS using Halogen subscription
    { emitter: reminderEmitter, listener: reminderListener } <- liftEffect HS.create
    _ <- H.subscribe reminderEmitter
    -- When JS fires a reminder drop event, notify the listener which emits an action
    liftEffect $ void $ Drag.subscribeToDrops \dropEvent ->
      HS.notify reminderListener (HandleReminderDrop dropEvent)
    -- Subscribe to role drop events from JS using Halogen subscription
    { emitter: roleEmitter, listener: roleListener } <- liftEffect HS.create
    _ <- H.subscribe roleEmitter
    -- When JS fires a role drop event, notify the listener which emits an action
    liftEffect $ void $ RoleDrag.subscribeToDrops \dropEvent ->
      HS.notify roleListener (HandleRoleDrop dropEvent)

  ReceiveAtoms atomStrings -> do
    currentState <- H.get
    let parsedAtoms = ASP.parseAnswerSetWithOriginals atomStrings
    let atoms = map _.atom parsedAtoms
    let timeline = ASP.extractTimelineWithSources parsedAtoms
    let allTimes = getAllTimePoints atoms
    -- Preserve selected time if it exists, otherwise find closest earlier time
    let preservedTime = case currentState.selectedTime of
          Just t | elem t allTimes -> Just t  -- Exact match exists
          Just t ->
            -- Find closest earlier time point (last one that's <= selected)
            let earlierTimes = filter (\tp -> ASP.compareTimePoints tp t /= GT) allTimes
            in case last earlierTimes of
                 Just closest -> Just closest
                 Nothing -> head allTimes  -- No earlier time, use first
          Nothing -> head allTimes
    H.modify_ \s -> s
      { parsedAtoms = parsedAtoms
      , atoms = atoms
      , timeline = timeline
      , allTimePoints = allTimes
      , selectedTime = preservedTime
      }

  SelectTimePoint t -> do
    state <- H.get
    H.modify_ \s -> s { selectedTime = Just t }
    -- Find the time atom's original string to enable scrolling to it
    let timeAtomSource = findTimeAtomSource state.parsedAtoms t
    -- Emit output event to scroll to the time atom in the answer set
    H.raise $ TimelineEventClicked
      { sourceAtom: fromMaybe (show t) timeAtomSource  -- Fallback to show representation
      , predicateName: "time"
      , predicateArity: 1
      }

  ClickTimelineEvent event -> do
    -- Extract source atom and predicate info from the event
    let eventInfo = case event of
          ASP.RoleAction r ->
            { sourceAtom: r.sourceAtom
            , predicateName: r.eventType  -- "d_st_tells" or "d_player_chooses"
            , predicateArity: 4
            }
          ASP.TokenPlaced r ->
            { sourceAtom: r.sourceAtom
            , predicateName: "reminder_on"
            , predicateArity: 3
            }
          ASP.Execution r ->
            { sourceAtom: r.sourceAtom
            , predicateName: "d_executed"
            , predicateArity: 2
            }
          ASP.Death r ->
            { sourceAtom: r.sourceAtom
            , predicateName: "d_died"
            , predicateArity: 2
            }
    -- Emit the output event
    H.raise $ TimelineEventClicked eventInfo

  -- Mouse events for SVG view (halogen-svg-elems doesn't support pointer events)
  StartDragReminderMouse { reminder, event } -> do
    liftEffect $ preventDefault (ME.toEvent event)
    let mouseX = toNumber (ME.clientX event)
    let mouseY = toNumber (ME.clientY event)
    H.modify_ \s -> s
      { dragging = Just
          { reminder
          , startX: mouseX
          , startY: mouseY
          , currentX: mouseX
          , currentY: mouseY
          , targetPlayer: Just reminder.player
          }
      }

  DragMoveMouse event -> do
    liftEffect $ preventDefault (ME.toEvent event)
    state <- H.get
    case state.dragging of
      Nothing -> pure unit
      Just ds -> do
        let mouseX = toNumber (ME.clientX event)
        let mouseY = toNumber (ME.clientY event)
        let gameState = case state.selectedTime of
              Just t -> ASP.buildGameState state.atoms t
              Nothing -> ASP.buildGameState state.atoms (ASP.Night 1 0 0)
        let targetPlayer = findClosestPlayer mouseX mouseY 250.0 250.0 180.0 (length gameState.players) gameState.players
        H.modify_ \s -> s
          { dragging = Just ds
              { currentX = mouseX
              , currentY = mouseY
              , targetPlayer = targetPlayer
              }
          }

  EndDragMouse event -> do
    liftEffect $ preventDefault (ME.toEvent event)
    state <- H.get
    case state.dragging of
      Nothing -> pure unit
      Just ds -> do
        H.modify_ \s -> s { dragging = Nothing }
        case ds.targetPlayer of
          Just toPlayer | toPlayer /= ds.reminder.player -> do
            case state.selectedTime of
              Just time ->
                H.raise $ ReminderMoved
                  { token: ds.reminder.token
                  , fromPlayer: ds.reminder.player
                  , toPlayer
                  , time
                  }
              Nothing -> pure unit
          _ -> pure unit

  CancelDrag -> do
    H.modify_ \s -> s { dragging = Nothing }

  ToggleViewMode -> do
    H.modify_ \s -> s { viewMode = if s.viewMode == SvgView then HtmlView else SvgView }

  ToggleBagPanel -> do
    H.modify_ \s -> s { bagCollapsed = not s.bagCollapsed }

  ToggleScriptPanel -> do
    H.modify_ \s -> s { scriptCollapsed = not s.scriptCollapsed }

  HandleReminderDrop dropEvent -> do
    -- Handle drop event from JS drag handler
    state <- H.get
    case state.selectedTime of
      Just time ->
        H.raise $ ReminderMoved
          { token: dropEvent.token
          , fromPlayer: dropEvent.fromPlayer
          , toPlayer: dropEvent.toPlayer
          , time
          }
      Nothing -> pure unit

  HandleRoleDrop dropEvent -> do
    -- Handle role drop event from JS drag handler
    state <- H.get
    -- Use selected time if available, otherwise use Setup (pre-game)
    -- In pre-solve mode there's no timeline, so we use Setup
    let time = fromMaybe ASP.Setup state.selectedTime
    H.raise $ RoleMoved
      { role: dropEvent.role
      , fromPlayer: dropEvent.fromPlayer
      , toPlayer: dropEvent.toPlayer
      , time
      }

-- | Calculate grid dimensions for a hollow rectangle that fits n players on perimeter
-- For n players, perimeter = 2*cols + 2*rows - 4 = n
-- We want roughly square grids, so cols ≈ rows
calculateGridDimensions :: Int -> { cols :: Int, rows :: Int }
calculateGridDimensions n
  | n <= 4 = { cols: n, rows: 1 }  -- Single row for small player counts
  | n <= 6 = { cols: 3, rows: 2 }  -- 3x2 = perimeter of 6
  | n <= 8 = { cols: 3, rows: 3 }  -- 3x3 = perimeter of 8
  | n <= 10 = { cols: 4, rows: 3 } -- 4x3 = perimeter of 10
  | n <= 12 = { cols: 4, rows: 4 } -- 4x4 = perimeter of 12
  | n <= 14 = { cols: 5, rows: 4 } -- 5x4 = perimeter of 14
  | n <= 16 = { cols: 5, rows: 5 } -- 5x5 = perimeter of 16
  | otherwise = { cols: 6, rows: 5 } -- 6x5 = perimeter of 18 (max supported nicely)

-- | Assign grid positions to players around the perimeter (clockwise from top-left)
-- Returns array of {row, col} for each player index
-- Note: PureScript's `..` operator returns descending range when start > end (e.g., 1..0 = [1,0]),
-- so we must guard against that for rightCol/leftCol which should be empty for small grids.
assignPerimeterPositions :: Int -> Int -> Int -> Array { row :: Int, col :: Int }
assignPerimeterPositions playerCount cols rows =
  let
    -- Walk around the perimeter: top row (left to right), right column (top to bottom),
    -- bottom row (right to left), left column (bottom to top)
    topRow = map (\c -> { row: 0, col: c }) (0 .. (cols - 1))

    -- Right column: rows 1 to (rows-2), i.e., middle rows only (excludes corners)
    -- For a 2-row grid, this should be empty (no middle rows)
    rightCol = if rows <= 2
               then []
               else map (\r -> { row: r, col: cols - 1 }) (1 .. (rows - 2))

    bottomRow = map (\c -> { row: rows - 1, col: c }) (Array.reverse (0 .. (cols - 1)))

    -- Left column: rows (rows-2) down to 1, i.e., middle rows only (excludes corners)
    -- For a 2-row grid, this should be empty (no middle rows)
    leftCol = if rows <= 2
              then []
              else map (\r -> { row: r, col: 0 }) (Array.reverse (1 .. (rows - 2)))

    allPositions = topRow <> rightCol <> bottomRow <> leftCol
  in
    take playerCount allPositions

-- | Debug version that returns component info
assignPerimeterPositionsDebug :: Int -> Int -> Int -> { positions :: Array { row :: Int, col :: Int }, debug :: String }
assignPerimeterPositionsDebug playerCount cols rows =
  let
    topRow = map (\c -> { row: 0, col: c }) (0 .. (cols - 1))

    -- Right column: rows 1 to (rows-2), i.e., middle rows only (excludes corners)
    -- For a 2-row grid, this should be empty (no middle rows)
    rightCol = if rows <= 2
               then []
               else map (\r -> { row: r, col: cols - 1 }) (1 .. (rows - 2))

    bottomRow = map (\c -> { row: rows - 1, col: c }) (Array.reverse (0 .. (cols - 1)))

    -- Left column: rows (rows-2) down to 1, i.e., middle rows only (excludes corners)
    -- For a 2-row grid, this should be empty (no middle rows)
    leftCol = if rows <= 2
              then []
              else map (\r -> { row: r, col: 0 }) (Array.reverse (1 .. (rows - 2)))

    allPositions = topRow <> rightCol <> bottomRow <> leftCol

    showPos p = "(" <> show p.row <> "," <> show p.col <> ")"
    showPosArr arr = "[" <> intercalate "," (map showPos arr) <> "]"

    debugStr = "cols=" <> show cols <> " rows=" <> show rows
            <> " | topRow=" <> showPosArr topRow
            <> " | rightCol=" <> showPosArr rightCol
            <> " | bottomRow=" <> showPosArr bottomRow
            <> " | leftCol=" <> showPosArr leftCol
            <> " | all=" <> showPosArr allPositions
  in
    { positions: take playerCount allPositions, debug: debugStr }

-- | Render the hollow grid with players on perimeter and empty center
renderHollowGrid :: forall cs m.
  Array { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  Maybe ASP.TimePoint ->  -- selected time for data attributes
  Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean } ->
  Array { row :: Int, col :: Int } ->
  Int ->  -- cols
  Int ->  -- rows
  Array (H.ComponentHTML Action cs m)
renderHollowGrid reminders selectedTime players positions cols rows =
  let
    -- Create a lookup from position to player
    positionedPlayers = Array.zipWith (\pos player -> { pos, player }) positions players
    -- Debug: show what positions we have
    positionStrs = map (\p -> "(" <> show p.row <> "," <> show p.col <> ")") positions
    -- Generate all grid cells
    allCells = do
      r <- 0 .. (rows - 1)
      c <- 0 .. (cols - 1)
      pure { row: r, col: c }
    -- For each cell, either render a player or empty/center cell
    renderCell cell =
      let
        foundPlayer = Array.find (\pp -> pp.pos.row == cell.row && pp.pos.col == cell.col) positionedPlayers
        cellDebug = "cell(" <> show cell.row <> "," <> show cell.col <> ")"
      in case foundPlayer of
        Just { player } ->
          HH.div
            [ HP.attr (HH.AttrName "data-cell") cellDebug
            , HP.attr (HH.AttrName "data-player") player.name
            ]
            [ renderHtmlPlayer reminders selectedTime player ]
        Nothing ->
          -- Empty center cell
          if cell.row > 0 && cell.row < rows - 1 && cell.col > 0 && cell.col < cols - 1
            then HH.div
              [ HP.style "background: rgba(0,0,0,0.05); border-radius: 8px; min-height: 60px;"
              , HP.attr (HH.AttrName "data-cell") cellDebug
              , HP.attr (HH.AttrName "data-empty") "center"
              ]
              []
            else HH.div
              [ HP.attr (HH.AttrName "data-cell") cellDebug
              , HP.attr (HH.AttrName "data-empty") "edge"
              , HP.style "min-height: 20px; background: rgba(255,0,0,0.1);"  -- Debug: highlight empty edge cells
              ]
              [ HH.text $ "empty:" <> cellDebug ]  -- Debug: show which cells are empty
  in
    -- Add positions debug before cells
    [ HH.div
        [ HP.style "font-size: 9px; color: #999; margin-bottom: 4px;" ]
        [ HH.text $ "Positions: " <> intercalate " " positionStrs ]
    ] <> map renderCell allCells

-- | Find the closest player to a given screen position
findClosestPlayer :: forall r.
  Number ->  -- mouseX
  Number ->  -- mouseY
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- playerCount
  Array { name :: String | r } ->
  Maybe String
findClosestPlayer mouseX mouseY centerX centerY radius playerCount players =
  let
    -- Calculate positions for all players
    playerDists = mapWithIndex calcDist players
    calcDist idx p =
      let
        angle = (toNumber idx) * 2.0 * pi / (toNumber playerCount) - pi / 2.0
        px = centerX + radius * cos angle
        py = centerY + radius * sin angle
        dx = mouseX - px
        dy = mouseY - py
        dist = sqrt (dx * dx + dy * dy)
      in { name: p.name, dist }
    -- Find the closest one (within a threshold of 50 pixels)
    closest = minimumBy (comparing _.dist) playerDists
  in case closest of
    Just p | p.dist < 60.0 -> Just p.name
    _ -> Nothing

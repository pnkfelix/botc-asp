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
import Data.Ord (comparing)
import Data.String (Pattern(..))
import Data.String.CodeUnits (toCharArray)
import Data.String as S
import Data.Number (sqrt)
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
import Data.Number (cos, sin, pi)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME
import Web.PointerEvent (PointerEvent)
import Web.PointerEvent as PE
import ElementHitTest (findPlayerAtPoint)

-- | View mode for grimoire display
data ViewMode = SvgView | HtmlView

derive instance eqViewMode :: Eq ViewMode

-- | Output events from the component
data Output
  = TimelineEventClicked
      { sourceAtom :: String      -- Original atom string to highlight in answer set
      , predicateName :: String   -- Predicate name for finding rules (e.g., "st_tells")
      , predicateArity :: Int     -- Arity for finding rules
      }
  | ReminderMoved
      { token :: String           -- The reminder token being moved (e.g., "poi_poisoned")
      , fromPlayer :: String      -- Original player the token was on
      , toPlayer :: String        -- New player the token is being moved to
      , time :: ASP.TimePoint     -- The time point at which this reminder applies
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
  }

-- | Component query type (empty - no queries supported)
data Query a

-- | Component actions
data Action
  = ReceiveAtoms (Array String)
  | SelectTimePoint ASP.TimePoint
  | ClickTimelineEvent ASP.TimelineEvent  -- User clicked on a specific event
  | ToggleViewMode                        -- Switch between SVG and HTML views
  -- Mouse events for SVG view (halogen-svg-elems doesn't support pointer/touch events)
  | StartDragReminderMouse { reminder :: { token :: String, player :: String, placedAt :: ASP.TimePoint }, event :: MouseEvent }
  | DragMoveMouse MouseEvent
  | EndDragMouse MouseEvent
  -- Pointer events for HTML view (unified: works for mouse, touch, and pen)
  | StartDragReminderPointer { reminder :: { token :: String, player :: String, placedAt :: ASP.TimePoint }, event :: PointerEvent }
  | DragMovePointer PointerEvent
  | EndDragPointer PointerEvent
  | CancelDrag

-- | The Halogen component with output events
component :: forall m. MonadEffect m => H.Component Query (Array String) Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< ReceiveAtoms
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
    }

-- | Get all unique time points from atoms
getAllTimePoints :: Array ASP.Atom -> Array ASP.TimePoint
getAllTimePoints atoms =
  nub $ sortBy ASP.compareTimePoints $ Array.mapMaybe getTimeFromAtom atoms
  where
    getTimeFromAtom (ASP.Time t) = Just t
    getTimeFromAtom (ASP.StTells _ _ _ t) = Just t
    getTimeFromAtom (ASP.PlayerChooses _ _ _ t) = Just t
    getTimeFromAtom (ASP.ReminderOn _ _ t) = Just t
    getTimeFromAtom (ASP.Alive _ t) = Just t
    getTimeFromAtom (ASP.Dead _ t) = Just t
    getTimeFromAtom (ASP.ActingRole t _) = Just t
    getTimeFromAtom _ = Nothing

-- | Main render function
render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div
    [ HP.style "display: flex; gap: 20px; margin-top: 20px; flex-wrap: wrap;" ]
    [ -- Timeline panel (left)
      HH.div
        [ HP.style "flex: 1; min-width: 300px; max-width: 400px;" ]
        [ HH.h3
            [ HP.style "margin: 0 0 10px 0; color: #333;" ]
            [ HH.text "Timeline" ]
        , renderTimeline state
        ]
    -- Grimoire panel (right)
    , HH.div
        [ HP.style $ "flex: 2; min-width: 400px;"
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
            [ HH.text $ "Drag reminder tokens to move them between players"
                <> if state.viewMode == HtmlView then " (touch supported)" else ""
            ]
        , if state.viewMode == SvgView
            then renderGrimoire state
            else renderHtmlGrimoire state
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
          [ HH.text "No timeline data available. Add #show directives for time/1, st_tells/4, player_chooses/4, etc." ]
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
eventAtTime _ (ASP.Execution _) = false  -- Executions happen at day end

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
                <> "background: " <> (if r.eventType == "st_tells" then "#e8f5e9" else "#fff3e0") <> "; "
                <> "margin-right: 6px;"
            ]
            [ HH.text $ formatRoleName r.role ]
        , HH.text $ if r.eventType == "st_tells"
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
    -- Calculate player positions for target detection
    playerPositions = mapWithIndex (calcPlayerPosition centerX centerY radius playerCount) gameState.players
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
          , HH.span_
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; border: 3px solid #9e9e9e; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Dead"
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
  { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean } ->
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
  in
    SE.g
      [ SA.transform [ SA.Translate rx ry ] ]
      [ -- Reminder circle - attach drag handler here (circles have geometry, receive events)
        SE.circle
          [ SA.cx 0.0
          , SA.cy 0.0
          , SA.r 12.0
          , SA.fill (Named (getReminderColor reminder.token))
          , SA.stroke (Named "#fff")
          , SA.strokeWidth 1.0
          , HP.style "cursor: grab;"
          , HE.onMouseDown \evt -> StartDragReminderMouse { reminder, event: evt }
          ]
      -- Reminder abbreviation - let clicks/touches pass through to circle
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
  Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean } ->
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

-- | Render HTML-based grimoire with grid layout (supports pointer events for mouse/touch/pen)
renderHtmlGrimoire :: forall cs m. State -> H.ComponentHTML Action cs m
renderHtmlGrimoire state =
  let
    gameState = case state.selectedTime of
      Just t -> ASP.buildGameState state.atoms t
      Nothing -> ASP.buildGameState state.atoms (ASP.Night 1 0 0)
    playerCount = length gameState.players
    isDragging = isJust state.dragging
  in
    HH.div
      [ HP.style "background: #f5f5f5; border-radius: 8px; padding: 15px;" ]
      [ -- Player grid
        HH.div
          ( [ HP.style $ "display: grid; grid-template-columns: repeat(auto-fill, minmax(120px, 1fr)); "
                <> "gap: 12px; min-height: 200px;"
                <> if isDragging then " touch-action: none;" else ""
            ] <> (if isDragging
                  then [ HE.onPointerMove DragMovePointer
                       , HE.onPointerUp EndDragPointer
                       , HE.onPointerLeave \_ -> CancelDrag
                       , HE.onPointerCancel \_ -> CancelDrag
                       ]
                  else [])
          )
          ( if playerCount > 0
              then map (renderHtmlPlayer gameState.reminders state.dragging) gameState.players
              else [ HH.div
                       [ HP.style "grid-column: 1 / -1; text-align: center; color: #999; padding: 40px;" ]
                       [ HH.text "No player data - add #show chair/2." ]
                   ]
          )
      -- Drag feedback (floating token)
      , renderHtmlDragFeedback state.dragging
      -- Compact legend
      , HH.div
          [ HP.style "margin-top: 12px; font-size: 11px; color: #666; text-align: center;" ]
          [ HH.span [ HP.style "margin-right: 10px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; border: 2px solid #4CAF50; margin-right: 3px; vertical-align: middle;" ] []
              , HH.text "Alive"
              ]
          , HH.span_
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; border: 2px solid #9e9e9e; margin-right: 3px; vertical-align: middle;" ] []
              , HH.text "Dead"
              ]
          ]
      ]

-- | Render a single player card in HTML grid view
renderHtmlPlayer :: forall cs m.
  Array { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  Maybe DragState ->
  { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean } ->
  H.ComponentHTML Action cs m
renderHtmlPlayer reminders dragState player =
  let
    playerReminders = filter (\r -> r.player == player.name && not (isDraggingReminder dragState r)) reminders
    aliveColor = if player.alive then "#4CAF50" else "#9e9e9e"
    roleColor = getRoleColor player.role
    isDropTarget = case dragState of
      Just ds -> ds.targetPlayer == Just player.name
      Nothing -> false
  in
    HH.div
      [ HP.style $ "background: " <> roleColor <> "; border-radius: 8px; padding: 10px; "
          <> "border: 3px solid " <> (if isDropTarget then "#FFD700" else aliveColor) <> "; "
          <> "text-align: center; min-height: 80px; "
          <> (if isDropTarget then "box-shadow: 0 0 10px #FFD700; " else "")
      , HP.attr (HH.AttrName "data-player") player.name
      ]
      [ -- Player name
        HH.div
          [ HP.style "color: white; font-weight: bold; font-size: 12px; margin-bottom: 2px;" ]
          [ HH.text player.name ]
      -- Role name
      , HH.div
          [ HP.style "color: rgba(255,255,255,0.9); font-size: 10px;" ]
          [ HH.text $ formatRoleName player.role ]
      -- Token (if different from role)
      , if player.token /= player.role
          then HH.div
            [ HP.style "color: #ffeb3b; font-size: 9px; margin-top: 2px;" ]
            [ HH.text $ "(" <> formatRoleName player.token <> ")" ]
          else HH.text ""
      -- Reminder tokens
      , if null playerReminders
          then HH.text ""
          else HH.div
            [ HP.style "display: flex; flex-wrap: wrap; gap: 4px; justify-content: center; margin-top: 6px;" ]
            (map (renderHtmlReminderToken player.name) playerReminders)
      ]

-- | Render a single reminder token in HTML view (draggable)
renderHtmlReminderToken :: forall cs m.
  String ->  -- player name (for identifying drop target)
  { token :: String, player :: String, placedAt :: ASP.TimePoint } ->
  H.ComponentHTML Action cs m
renderHtmlReminderToken _playerName reminder =
  HH.div
    [ HP.style $ "width: 24px; height: 24px; border-radius: 50%; "
        <> "background: " <> getReminderColor reminder.token <> "; "
        <> "border: 1px solid white; display: flex; align-items: center; justify-content: center; "
        <> "cursor: grab; font-size: 7px; font-weight: bold; color: white; "
        <> "touch-action: none; user-select: none;"
    , HE.onPointerDown \evt -> StartDragReminderPointer { reminder, event: evt }
    ]
    [ HH.text $ abbreviateToken reminder.token ]

-- | Render floating drag feedback for HTML view
renderHtmlDragFeedback :: forall cs m. Maybe DragState -> H.ComponentHTML Action cs m
renderHtmlDragFeedback Nothing = HH.text ""
renderHtmlDragFeedback (Just ds) =
  HH.div
    [ HP.style $ "position: fixed; pointer-events: none; z-index: 1000; "
        <> "left: " <> show (ds.currentX - 14.0) <> "px; "
        <> "top: " <> show (ds.currentY - 14.0) <> "px; "
        <> "width: 28px; height: 28px; border-radius: 50%; "
        <> "background: " <> getReminderColor ds.reminder.token <> "; "
        <> "border: 2px solid #FFD700; "
        <> "display: flex; align-items: center; justify-content: center; "
        <> "font-size: 8px; font-weight: bold; color: white; "
        <> "box-shadow: 0 2px 8px rgba(0,0,0,0.3);"
    ]
    [ HH.text $ abbreviateToken ds.reminder.token ]

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

isOutsider :: String -> Boolean
isOutsider r = r == "butler" || r == "drunk" || r == "recluse" || r == "saint"

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
formatTimePoint (ASP.Night n 0 0) = "Night " <> show n <> " (Setup)"
formatTimePoint (ASP.Night n r s) = "Night " <> show n <> " (" <> show r <> "." <> show s <> ")"
formatTimePoint (ASP.Day n "0") = "Day " <> show n <> " (Start)"
formatTimePoint (ASP.Day n "exec") = "Day " <> show n <> " (Execution)"
formatTimePoint (ASP.Day n p) = "Day " <> show n <> " (" <> p <> ")"
formatTimePoint (ASP.UnknownTime s) = s

-- | Handle actions
handleAction :: forall cs m. MonadEffect m => Action -> H.HalogenM State Action cs Output m Unit
handleAction = case _ of
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

  SelectTimePoint t ->
    H.modify_ \s -> s { selectedTime = Just t }

  ClickTimelineEvent event -> do
    -- Extract source atom and predicate info from the event
    let eventInfo = case event of
          ASP.RoleAction r ->
            { sourceAtom: r.sourceAtom
            , predicateName: r.eventType  -- "st_tells" or "player_chooses"
            , predicateArity: 4
            }
          ASP.TokenPlaced r ->
            { sourceAtom: r.sourceAtom
            , predicateName: "reminder_on"
            , predicateArity: 3
            }
          ASP.Execution r ->
            { sourceAtom: r.sourceAtom
            , predicateName: "executed"
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

  -- Pointer events for HTML view (works with mouse, touch, and pen)
  StartDragReminderPointer { reminder, event } -> do
    liftEffect $ preventDefault (PE.toEvent event)
    let pointerX = toNumber (PE.clientX event)
    let pointerY = toNumber (PE.clientY event)
    H.modify_ \s -> s
      { dragging = Just
          { reminder
          , startX: pointerX
          , startY: pointerY
          , currentX: pointerX
          , currentY: pointerY
          , targetPlayer: Just reminder.player
          }
      }

  DragMovePointer event -> do
    liftEffect $ preventDefault (PE.toEvent event)
    state <- H.get
    case state.dragging of
      Nothing -> pure unit
      Just ds -> do
        let pointerX = toNumber (PE.clientX event)
        let pointerY = toNumber (PE.clientY event)
        targetPlayer <- liftEffect $ findPlayerAtPoint pointerX pointerY
        H.modify_ \s -> s
          { dragging = Just ds
              { currentX = pointerX
              , currentY = pointerY
              , targetPlayer = targetPlayer
              }
          }

  EndDragPointer event -> do
    liftEffect $ preventDefault (PE.toEvent event)
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

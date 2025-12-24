module Component.TimelineGrimoire
  ( component
  , Query
  ) where

import Prelude

import AnswerSetParser as ASP
import Data.Array (filter, length, mapWithIndex, null, sortBy, nub, head, take)
import Data.Array as Array
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Void (Void)
import Data.Int (toNumber)
import Data.Ord (comparing)
import Data.String (Pattern(..), split, toUpper) as S
import Data.String (length) as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Math (cos, sin, pi)

-- | Component state
type State =
  { atoms :: Array ASP.Atom
  , timeline :: Array ASP.TimelineEvent
  , selectedTime :: Maybe ASP.TimePoint
  , allTimePoints :: Array ASP.TimePoint
  }

-- | Component query type (empty - no queries supported)
data Query a

-- | Component actions
data Action
  = ReceiveAtoms (Array String)
  | SelectTimePoint ASP.TimePoint

-- | The Halogen component (slot-less, no output)
component :: forall m. H.Component Query (Array String) Void m
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
    atoms = ASP.parseAnswerSet atomStrings
    timeline = ASP.extractTimeline atoms
    allTimes = getAllTimePoints atoms
    firstTime = head allTimes
  in
    { atoms
    , timeline
    , selectedTime: firstTime
    , allTimePoints: allTimes
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
        [ HP.style "flex: 2; min-width: 400px;" ]
        [ HH.h3
            [ HP.style "margin: 0 0 10px 0; color: #333;" ]
            [ HH.text $ "Grimoire" <> case state.selectedTime of
                Just t -> " @ " <> formatTimePoint t
                Nothing -> ""
            ]
        , renderGrimoire state
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

-- | Render a single event
renderEvent :: forall cs m. ASP.TimelineEvent -> H.ComponentHTML Action cs m
renderEvent event =
  case event of
    ASP.RoleAction r ->
      HH.div
        [ HP.style "font-size: 12px; color: #555; margin: 4px 0;" ]
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
        [ HP.style "font-size: 12px; color: #888; margin: 4px 0;" ]
        [ HH.text $ r.token <> " placed on " <> r.player ]
    ASP.Execution r ->
      HH.div
        [ HP.style "font-size: 12px; color: #c62828; margin: 4px 0; font-weight: bold;" ]
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
  in
    HH.div
      [ HP.style "background: #f5f5f5; border-radius: 8px; padding: 20px;" ]
      [ -- SVG grimoire
        HH.element (HH.ElemName "svg")
          [ HP.attr (HH.AttrName "viewBox") ("0 0 " <> show width <> " " <> show height)
          , HP.style "width: 100%; max-width: 500px; height: auto; display: block; margin: 0 auto;"
          ]
          (
            -- Draw lines connecting neighbors (circle)
            [ HH.element (HH.ElemName "circle")
                [ HP.attr (HH.AttrName "cx") (show centerX)
                , HP.attr (HH.AttrName "cy") (show centerY)
                , HP.attr (HH.AttrName "r") (show radius)
                , HP.attr (HH.AttrName "fill") "none"
                , HP.attr (HH.AttrName "stroke") "#ccc"
                , HP.attr (HH.AttrName "stroke-width") "1"
                , HP.attr (HH.AttrName "stroke-dasharray") "4,4"
                ]
                []
            ]
            -- Draw players
            <> (mapWithIndex (renderPlayer centerX centerY radius playerCount gameState.reminders) gameState.players)
          )
      -- Legend
      , HH.div
          [ HP.style "margin-top: 15px; font-size: 12px; color: #666; text-align: center;" ]
          [ HH.span [ HP.style "margin-right: 15px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #4CAF50; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Alive"
              ]
          , HH.span [ HP.style "margin-right: 15px;" ]
              [ HH.span [ HP.style "display: inline-block; width: 12px; height: 12px; border-radius: 50%; background: #9e9e9e; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Dead"
              ]
          , HH.span_
              [ HH.span [ HP.style "display: inline-block; width: 8px; height: 8px; border-radius: 50%; background: #ff9800; margin-right: 4px; vertical-align: middle;" ] []
              , HH.text "Reminder Token"
              ]
          ]
      ]

-- | Render a single player token
renderPlayer :: forall cs m.
  Number ->  -- centerX
  Number ->  -- centerY
  Number ->  -- radius
  Int ->     -- total players
  Array { token :: String, player :: String } ->  -- reminders
  Int ->     -- index
  { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean } ->
  H.ComponentHTML Action cs m
renderPlayer centerX centerY radius playerCount reminders idx player =
  let
    -- Calculate position on circle (start from top, go clockwise)
    angle = (toNumber idx) * 2.0 * pi / (toNumber playerCount) - pi / 2.0
    x = centerX + radius * cos angle
    y = centerY + radius * sin angle

    -- Player's reminders
    playerReminders = filter (\r -> r.player == player.name) reminders

    -- Colors
    aliveColor = if player.alive then "#4CAF50" else "#9e9e9e"
    roleColor = getRoleColor player.role
  in
    HH.element (HH.ElemName "g")
      [ HP.attr (HH.AttrName "transform") ("translate(" <> show x <> "," <> show y <> ")") ]
      ( [
        -- Main role token (outer circle)
        HH.element (HH.ElemName "circle")
          [ HP.attr (HH.AttrName "cx") "0"
          , HP.attr (HH.AttrName "cy") "0"
          , HP.attr (HH.AttrName "r") "35"
          , HP.attr (HH.AttrName "fill") roleColor
          , HP.attr (HH.AttrName "stroke") aliveColor
          , HP.attr (HH.AttrName "stroke-width") "3"
          ]
          []
      -- Player name
      , HH.element (HH.ElemName "text")
          [ HP.attr (HH.AttrName "x") "0"
          , HP.attr (HH.AttrName "y") "-8"
          , HP.attr (HH.AttrName "text-anchor") "middle"
          , HP.attr (HH.AttrName "font-size") "10"
          , HP.attr (HH.AttrName "fill") "white"
          , HP.attr (HH.AttrName "font-weight") "bold"
          ]
          [ HH.text player.name ]
      -- Role name
      , HH.element (HH.ElemName "text")
          [ HP.attr (HH.AttrName "x") "0"
          , HP.attr (HH.AttrName "y") "5"
          , HP.attr (HH.AttrName "text-anchor") "middle"
          , HP.attr (HH.AttrName "font-size") "8"
          , HP.attr (HH.AttrName "fill") "white"
          ]
          [ HH.text $ formatRoleName player.role ]
      -- Token (what they think they are, if different)
      , if player.token /= player.role
          then HH.element (HH.ElemName "text")
            [ HP.attr (HH.AttrName "x") "0"
            , HP.attr (HH.AttrName "y") "16"
            , HP.attr (HH.AttrName "text-anchor") "middle"
            , HP.attr (HH.AttrName "font-size") "7"
            , HP.attr (HH.AttrName "fill") "#ffeb3b"
            , HP.attr (HH.AttrName "font-style") "italic"
            ]
            [ HH.text $ "(" <> formatRoleName player.token <> ")" ]
          else HH.text ""
      -- Reminder tokens (small circles around the main token)
      ] <> mapWithIndex (renderReminderToken (length playerReminders)) playerReminders
      )

-- | Render a reminder token
renderReminderToken :: forall cs m.
  Int ->   -- total reminders for this player
  Int ->   -- index
  { token :: String, player :: String } ->
  H.ComponentHTML Action cs m
renderReminderToken total idx reminder =
  let
    -- Position reminder tokens in a small arc below the player token
    startAngle = pi / 4.0  -- Start 45 degrees from bottom
    angleSpread = pi / 2.0  -- Spread over 90 degrees
    angle = if total == 1
      then pi / 2.0  -- Single token at bottom
      else startAngle + (toNumber idx) * angleSpread / (toNumber (total - 1))
    dist = 48.0  -- Distance from center
    rx = dist * cos angle
    ry = dist * sin angle
  in
    HH.element (HH.ElemName "g")
      [ HP.attr (HH.AttrName "transform") ("translate(" <> show rx <> "," <> show ry <> ")") ]
      [ -- Reminder circle
        HH.element (HH.ElemName "circle")
          [ HP.attr (HH.AttrName "cx") "0"
          , HP.attr (HH.AttrName "cy") "0"
          , HP.attr (HH.AttrName "r") "12"
          , HP.attr (HH.AttrName "fill") (getReminderColor reminder.token)
          , HP.attr (HH.AttrName "stroke") "#fff"
          , HP.attr (HH.AttrName "stroke-width") "1"
          ]
          []
      -- Reminder abbreviation
      , HH.element (HH.ElemName "text")
          [ HP.attr (HH.AttrName "x") "0"
          , HP.attr (HH.AttrName "y") "3"
          , HP.attr (HH.AttrName "text-anchor") "middle"
          , HP.attr (HH.AttrName "font-size") "6"
          , HP.attr (HH.AttrName "fill") "white"
          , HP.attr (HH.AttrName "font-weight") "bold"
          ]
          [ HH.text $ abbreviateToken reminder.token ]
      ]

-- | Get color based on role type
getRoleColor :: String -> String
getRoleColor role
  | isMinion role = "#7b1fa2"  -- Purple for minions
  | isDemon role = "#c62828"    -- Red for demons
  | isOutsider role = "#0277bd" -- Blue for outsiders
  | otherwise = "#2e7d32"       -- Green for townsfolk

isMinion :: String -> Boolean
isMinion r = r == "poisoner" || r == "spy" || r == "scarlet_woman" || r == "baron"

isDemon :: String -> Boolean
isDemon r = r == "imp"

isOutsider :: String -> Boolean
isOutsider r = r == "butler" || r == "drunk" || r == "recluse" || r == "saint"

-- | Get color for reminder token
getReminderColor :: String -> String
getReminderColor token
  | token == "poi_poisoned" = "#9c27b0"  -- Purple for poison
  | token == "imp_dead" = "#f44336"       -- Red for death
  | token == "monk_protected" = "#4caf50" -- Green for protection
  | token == "ft_red_herring" = "#ff5722" -- Orange for red herring
  | otherwise = "#ff9800"                 -- Default orange

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
  _ -> takeChars 3 t
  where
    takeChars n s = if Str.length s <= n then s else fold (take n (S.split (S.Pattern "") s))

-- | Format role name for display (capitalize, replace underscores)
formatRoleName :: String -> String
formatRoleName role =
  let
    parts = S.split (S.Pattern "_") role
    capitalized = map capitalize parts
  in
    intercalate " " capitalized
  where
    capitalize s = case Array.uncons (S.split (S.Pattern "") s) of
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
handleAction :: forall cs o m. Action -> H.HalogenM State Action cs o m Unit
handleAction = case _ of
  ReceiveAtoms atomStrings -> do
    let atoms = ASP.parseAnswerSet atomStrings
    let timeline = ASP.extractTimeline atoms
    let allTimes = getAllTimePoints atoms
    let firstTime = head allTimes
    H.modify_ \s -> s
      { atoms = atoms
      , timeline = timeline
      , allTimePoints = allTimes
      , selectedTime = firstTime
      }

  SelectTimePoint t ->
    H.modify_ \s -> s { selectedTime = Just t }

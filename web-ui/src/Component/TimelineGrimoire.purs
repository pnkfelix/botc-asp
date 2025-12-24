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
import Data.String (Pattern(..))
import Data.String as S
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
        SE.svg
          [ SA.viewBox 0.0 0.0 width height
          , SA.width width
          , SA.height height
          ]
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
                then mapWithIndex (renderPlayer centerX centerY radius playerCount gameState.reminders) gameState.players
                else [ SE.text
                         [ SA.x centerX
                         , SA.y centerY
                         , SA.textAnchor AnchorMiddle
                         , SA.fill (Named "#999")
                         ]
                         [ HH.text "No player data - add #show chair/2." ]
                     ])
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

    -- Direction toward center (angle + pi points inward)
    angleToCenter = angle + pi

    -- Colors
    aliveColor = if player.alive then "#4CAF50" else "#9e9e9e"
    roleColor = getRoleColor player.role
  in
    SE.g
      [ SA.transform [ SA.Translate x y ] ]
      ( [
        -- Main role token (outer circle)
        SE.circle
          [ SA.cx 0.0
          , SA.cy 0.0
          , SA.r 35.0
          , SA.fill (Named roleColor)
          , SA.stroke (Named aliveColor)
          , SA.strokeWidth 3.0
          ]
      -- Player name
      , SE.text
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

-- | Render a reminder token
renderReminderToken :: forall cs m.
  Number -> -- angle toward center
  Int ->    -- total reminders for this player
  Int ->    -- index
  { token :: String, player :: String } ->
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
      [ -- Reminder circle
        SE.circle
          [ SA.cx 0.0
          , SA.cy 0.0
          , SA.r 12.0
          , SA.fill (Named (getReminderColor reminder.token))
          , SA.stroke (Named "#fff")
          , SA.strokeWidth 1.0
          ]
      -- Reminder abbreviation
      , SE.text
          [ SA.x 0.0
          , SA.y 3.0
          , SA.textAnchor AnchorMiddle
          , SA.fill (Named "white")
          , SA.fontWeight FWeightBold
          , SA.fontSize (FontSizeLength (Px 6.0))
          ]
          [ HH.text $ abbreviateToken reminder.token ]
      ]

-- | Get unique color for each role
-- Good (Townsfolk, Outsiders): blue-family tones (no red)
-- Evil (Minions, Demons): red-family tones (no blue)
getRoleColor :: String -> String
getRoleColor role = case role of
  -- Townsfolk (blue spectrum, greens, purples - no red)
  "washerwoman"    -> "#2196F3"  -- Bright blue
  "librarian"      -> "#1976D2"  -- Medium blue
  "investigator"   -> "#0D47A1"  -- Deep blue
  "chef"           -> "#00BCD4"  -- Cyan
  "empath"         -> "#00ACC1"  -- Dark cyan
  "fortune_teller" -> "#673AB7"  -- Deep purple
  "undertaker"     -> "#512DA8"  -- Dark purple
  "monk"           -> "#689F38"  -- Light green (no red)
  "ravenkeeper"    -> "#00796B"  -- Dark teal
  "virgin"         -> "#03A9F4"  -- Light blue
  "slayer"         -> "#546E7A"  -- Blue grey
  "soldier"        -> "#37474F"  -- Dark blue grey
  "mayor"          -> "#3F51B5"  -- Indigo
  -- Outsiders (teal/cyan family - no red)
  "butler"         -> "#26A69A"  -- Teal
  "drunk"          -> "#4DB6AC"  -- Light teal
  "recluse"        -> "#78909C"  -- Blue grey light
  "saint"          -> "#4DD0E1"  -- Light cyan
  -- Minions (orange/amber family - no blue)
  "poisoner"       -> "#E65100"  -- Deep orange
  "spy"            -> "#FF9800"  -- Amber
  "scarlet_woman"  -> "#E53935"  -- Scarlet red
  "baron"          -> "#8D6E63"  -- Brown
  -- Demon (dark red - no blue)
  "imp"            -> "#B71C1C"  -- Dark crimson
  -- Fallback by type
  _ | isMinion role  -> "#E65100"
    | isDemon role   -> "#B71C1C"
    | isOutsider role -> "#26A69A"
    | otherwise      -> "#2196F3"

-- | Get lighter shade for reminder tokens - uses same hue as source role
-- (Reminder tokens use the same color as the role they come from)
getRoleColorLight :: String -> String
getRoleColorLight role = case role of
  -- Townsfolk (lighter versions)
  "washerwoman"    -> "#64B5F6"  -- Light blue
  "librarian"      -> "#42A5F5"  -- Light medium blue
  "investigator"   -> "#5472D3"  -- Light deep blue
  "chef"           -> "#4DD0E1"  -- Light cyan
  "empath"         -> "#4DD0E1"  -- Light cyan
  "fortune_teller" -> "#9575CD"  -- Light purple
  "undertaker"     -> "#7E57C2"  -- Light dark purple
  "monk"           -> "#9CCC65"  -- Light green
  "ravenkeeper"    -> "#4DB6AC"  -- Light teal
  "virgin"         -> "#4FC3F7"  -- Lighter blue
  "slayer"         -> "#90A4AE"  -- Light blue grey
  "soldier"        -> "#78909C"  -- Light dark grey
  "mayor"          -> "#7986CB"  -- Light indigo
  -- Outsiders (lighter versions)
  "butler"         -> "#80CBC4"  -- Light teal
  "drunk"          -> "#B2DFDB"  -- Lighter teal
  "recluse"        -> "#B0BEC5"  -- Lighter blue grey
  "saint"          -> "#80DEEA"  -- Lighter cyan
  -- Minions (lighter versions)
  "poisoner"       -> "#FF8A65"  -- Light orange
  "spy"            -> "#FFB74D"  -- Light amber
  "scarlet_woman"  -> "#EF5350"  -- Light red
  "baron"          -> "#BCAAA4"  -- Light brown
  -- Demon
  "imp"            -> "#EF5350"  -- Light red
  -- Fallback
  _ | isMinion role  -> "#FF8A65"
    | isDemon role   -> "#EF5350"
    | isOutsider role -> "#80CBC4"
    | otherwise      -> "#64B5F6"

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

-- | Get color for reminder token (shade of source role's type color)
getReminderColor :: String -> String
getReminderColor token = getRoleColorLight (tokenToRole token)

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

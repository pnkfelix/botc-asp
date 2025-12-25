module Component.TimelineGrimoire
  ( component
  , Query
  , Output(..)
  ) where

import Prelude

import AnswerSetParser as ASP
import Data.Array (elem, filter, length, map, mapWithIndex, null, sortBy, nub, head, last, take, foldl)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (fold, intercalate)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Int (toNumber)
import Data.Ord (comparing)
import Data.String (Pattern(..))
import Data.String.CodeUnits (toCharArray)
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

-- | Output events from the component
data Output
  = TimelineEventClicked
      { sourceAtom :: String      -- Original atom string to highlight in answer set
      , predicateName :: String   -- Predicate name for finding rules (e.g., "st_tells")
      , predicateArity :: Int     -- Arity for finding rules
      }

-- | Component state
type State =
  { parsedAtoms :: Array ASP.ParsedAtom  -- Parsed atoms with original strings
  , atoms :: Array ASP.Atom              -- Just the atoms (for backward compat)
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
  | ClickTimelineEvent ASP.TimelineEvent  -- User clicked on a specific event

-- | The Halogen component with output events
component :: forall m. H.Component Query (Array String) Output m
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

-- | Render a single event (clickable for navigation)
renderEvent :: forall cs m. ASP.TimelineEvent -> H.ComponentHTML Action cs m
renderEvent event =
  case event of
    ASP.RoleAction r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #555; margin: 4px 0; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to navigate to this atom in the answer set and its rule definition"
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
        , HP.title "Click to navigate to this atom"
        ]
        [ HH.text $ r.token <> " placed on " <> r.player ]
    ASP.Execution r ->
      HH.div
        [ HP.style $ "font-size: 12px; color: #c62828; margin: 4px 0; font-weight: bold; cursor: pointer; "
            <> "padding: 4px 6px; border-radius: 4px; transition: background-color 0.2s;"
        , HE.onClick \_ -> ClickTimelineEvent event
        , HP.title "Click to navigate to this atom"
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
  Array { token :: String, player :: String, placedAt :: ASP.TimePoint } ->  -- reminders (sorted by placedAt)
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
handleAction :: forall cs m. Action -> H.HalogenM State Action cs Output m Unit
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

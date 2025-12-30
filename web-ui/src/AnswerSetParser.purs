module AnswerSetParser
  ( Atom(..)
  , TimePoint(..)
  , GameState
  , TimelineEvent(..)
  , ParsedAtom
  , parseAnswerSet
  , parseAnswerSetWithOriginals
  , buildGameState
  , extractTimeline
  , extractTimelineWithSources
  , getStateAtTime
  , compareTimePoints
  , atomToPredicateName
  ) where

import Prelude

import Data.Array (filter, mapMaybe, sortBy, nub, head, findIndex, index)
import Data.Foldable (elem, all, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim, indexOf, lastIndexOf, length, drop, take)
import Data.Ord (comparing)

-- | A parsed atom from the answer set
data Atom
  = Assigned Int String String          -- assigned(time, player, role)
  | Received String String              -- received(player, token)
  | StTells String String String TimePoint  -- st_tells(role, player, message, time)
  | PlayerChooses String String String TimePoint  -- player_chooses(role, player, choice, time)
  | ReminderOn String String TimePoint  -- reminder_on(token, player, time)
  | Alive String TimePoint              -- alive(player, time)
  | Dead String TimePoint               -- dead(player, time)
  | GhostVoteUsed String TimePoint      -- ghost_vote_used(player, time) - player has used their ghost vote
  | Time TimePoint                      -- time(timepoint)
  | ActingRole TimePoint String         -- acting_role(time, role)
  | Chair String Int                    -- game_chair(player, position)
  | Executed String Int                 -- executed(player, day)
  | UnknownAtom String                  -- anything we don't recognize

derive instance eqAtom :: Eq Atom

-- | A parsed atom with its original string representation
type ParsedAtom = { atom :: Atom, original :: String }

-- | Time points in the game
data TimePoint
  = Night Int Int Int    -- night(nightNum, roleOrder, substep)
  | Day Int String       -- day(dayNum, phase) where phase is "0" or "exec"
  | UnknownTime String

derive instance eqTimePoint :: Eq TimePoint

-- Custom Ord instance to properly interleave nights and days:
-- Night 1 < Day 1 < Night 2 < Day 2, etc.
instance ordTimePoint :: Ord TimePoint where
  compare (Night n1 r1 s1) (Night n2 r2 s2) =
    case compare n1 n2 of
      EQ -> case compare r1 r2 of
        EQ -> compare s1 s2
        other -> other
      other -> other
  compare (Day d1 p1) (Day d2 p2) =
    case compare d1 d2 of
      EQ -> compare p1 p2
      other -> other
  compare (Night n _r _s) (Day d _p) =
    -- Night n comes before Day n, but after Day (n-1)
    if n <= d then LT else GT
  compare (Day d _p) (Night n _r _s) =
    -- Day d comes after Night d, but before Night (d+1)
    if d < n then LT else GT
  compare (UnknownTime s1) (UnknownTime s2) = compare s1 s2
  compare (UnknownTime _) _ = GT
  compare _ (UnknownTime _) = LT

instance showTimePoint :: Show TimePoint where
  show (Night n r s) = "night(" <> show n <> "," <> show r <> "," <> show s <> ")"
  show (Day n p) = "day(" <> show n <> "," <> p <> ")"
  show (UnknownTime s) = s

-- | Compare time points for ordering
compareTimePoints :: TimePoint -> TimePoint -> Ordering
compareTimePoints t1 t2 = compare t1 t2

-- | An event in the timeline
data TimelineEvent
  = RoleAction
      { time :: TimePoint
      , role :: String
      , eventType :: String  -- "st_tells" or "player_chooses"
      , player :: String
      , message :: String
      , sourceAtom :: String  -- Original atom string for navigation
      }
  | TokenPlaced
      { time :: TimePoint
      , token :: String
      , player :: String
      , sourceAtom :: String
      }
  | Execution
      { day :: Int
      , player :: String
      , sourceAtom :: String
      }

derive instance eqTimelineEvent :: Eq TimelineEvent

-- | Game state at a particular time point
type GameState =
  { players :: Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean }
  , reminders :: Array { token :: String, player :: String, placedAt :: TimePoint }
  , time :: TimePoint
  }

-- | Parse a single atom string into structured data
parseAtom :: String -> Atom
parseAtom atomStr =
  let
    trimmed = trim atomStr
  in
    fromMaybe (UnknownAtom trimmed) $
      parseAssigned trimmed <|>
      parseReceived trimmed <|>
      parseStTells trimmed <|>
      parsePlayerChooses trimmed <|>
      parseReminderOn trimmed <|>
      parseAlive trimmed <|>
      parseDead trimmed <|>
      parseGhostVoteUsed trimmed <|>
      parseTimeAtom trimmed <|>
      parseActingRole trimmed <|>
      parseChair trimmed <|>
      parseExecuted trimmed

-- | Parse assigned(T, Player, Role)
parseAssigned :: String -> Maybe Atom
parseAssigned s = do
  let pattern = "assigned("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)  -- Remove "assigned(" and ")"
  let parts = split (Pattern ",") rest
  case parts of
    [t, p, r] -> Just $ Assigned (fromMaybe 0 $ parseInt (trim t)) (trim p) (trim r)
    _ -> Nothing

-- | Parse received(Player, Token)
parseReceived :: String -> Maybe Atom
parseReceived s = do
  let pattern = "received("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  let parts = split (Pattern ",") rest
  case parts of
    [p, t] -> Just $ Received (trim p) (trim t)
    _ -> Nothing

-- | Parse st_tells(Role, Player, Message, Time)
parseStTells :: String -> Maybe Atom
parseStTells s = do
  let pattern = "st_tells("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  -- Handle nested parentheses in message and time
  parseStTellsArgs rest

parseStTellsArgs :: String -> Maybe Atom
parseStTellsArgs s =
  let
    -- Find first comma (after role)
    parts1 = splitAtFirstComma s
  in case parts1 of
    Just { before: role, after: rest1 } ->
      case splitAtFirstComma rest1 of
        Just { before: player, after: rest2 } ->
          -- rest2 contains "message, time)" - need to find the time which is at the end
          case parseMessageAndTime rest2 of
            Just { message, time } ->
              Just $ StTells (trim role) (trim player) message time
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- | Parse player_chooses(Role, Player, Choice, Time)
parsePlayerChooses :: String -> Maybe Atom
parsePlayerChooses s = do
  let pattern = "player_chooses("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  parsePlayerChoosesArgs rest

parsePlayerChoosesArgs :: String -> Maybe Atom
parsePlayerChoosesArgs s =
  let
    parts1 = splitAtFirstComma s
  in case parts1 of
    Just { before: role, after: rest1 } ->
      case splitAtFirstComma rest1 of
        Just { before: player, after: rest2 } ->
          case parseMessageAndTime rest2 of
            Just { message, time } ->
              Just $ PlayerChooses (trim role) (trim player) message time
            Nothing -> Nothing
        Nothing -> Nothing
    Nothing -> Nothing

-- | Parse reminder_on(Token, Player, Time)
parseReminderOn :: String -> Maybe Atom
parseReminderOn s = do
  let pattern = "reminder_on("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  parseReminderOnArgs rest

parseReminderOnArgs :: String -> Maybe Atom
parseReminderOnArgs s =
  case splitAtFirstComma s of
    Just { before: token, after: rest1 } ->
      case splitAtFirstComma rest1 of
        Just { before: player, after: timeStr } ->
          Just $ ReminderOn (trim token) (trim player) (parseTime (trim timeStr))
        Nothing -> Nothing
    Nothing -> Nothing

-- | Parse alive(Player, Time)
parseAlive :: String -> Maybe Atom
parseAlive s = do
  let pattern = "alive("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  case splitAtFirstComma rest of
    Just { before: player, after: timeStr } ->
      Just $ Alive (trim player) (parseTime (trim timeStr))
    Nothing -> Nothing

-- | Parse dead(Player, Time)
parseDead :: String -> Maybe Atom
parseDead s = do
  let pattern = "dead("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  case splitAtFirstComma rest of
    Just { before: player, after: timeStr } ->
      Just $ Dead (trim player) (parseTime (trim timeStr))
    Nothing -> Nothing

-- | Parse ghost_vote_used(Player, Time)
parseGhostVoteUsed :: String -> Maybe Atom
parseGhostVoteUsed s = do
  let pattern = "ghost_vote_used("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  case splitAtFirstComma rest of
    Just { before: player, after: timeStr } ->
      Just $ GhostVoteUsed (trim player) (parseTime (trim timeStr))
    Nothing -> Nothing

-- | Parse time(TimePoint)
parseTimeAtom :: String -> Maybe Atom
parseTimeAtom s = do
  let pattern = "time("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  Just $ Time (parseTime (trim rest))

-- | Parse acting_role(Time, Role)
parseActingRole :: String -> Maybe Atom
parseActingRole s = do
  let pattern = "acting_role("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  case parseTimeAndRest rest of
    Just { time, rest: role } ->
      Just $ ActingRole time (trim role)
    Nothing -> Nothing

-- | Parse game_chair(Player, Position)
parseChair :: String -> Maybe Atom
parseChair s = do
  let pattern = "game_chair("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  let parts = split (Pattern ",") rest
  case parts of
    [p, pos] -> Just $ Chair (trim p) (fromMaybe 0 $ parseInt (trim pos))
    _ -> Nothing

-- | Parse executed(Player, Day)
parseExecuted :: String -> Maybe Atom
parseExecuted s = do
  let pattern = "executed("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  let parts = split (Pattern ",") rest
  case parts of
    [p, d] -> Just $ Executed (trim p) (fromMaybe 0 $ parseInt (trim d))
    _ -> Nothing

-- | Parse a time string like "night(1,2,3)" or "day(1,0)"
parseTime :: String -> TimePoint
parseTime s =
  let trimmed = trim s
  in fromMaybe (UnknownTime trimmed) $
    parseNightTime trimmed <|> parseDayTime trimmed

parseNightTime :: String -> Maybe TimePoint
parseNightTime s = do
  let pattern = "night("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)  -- Remove trailing )
  let parts = split (Pattern ",") rest
  case parts of
    [n, r, sub] ->
      let night = fromMaybe 1 $ parseInt (trim n)
          roleOrd = fromMaybe 0 $ parseInt (trim r)
          substep = fromMaybe 0 $ parseInt (trim sub)
      in Just $ Night night roleOrd substep
    _ -> Nothing

parseDayTime :: String -> Maybe TimePoint
parseDayTime s = do
  let pattern = "day("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)
  let parts = split (Pattern ",") rest
  case parts of
    [d, phase] -> Just $ Day (fromMaybe 1 $ parseInt (trim d)) (trim phase)
    _ -> Nothing

-- | Split at first comma (outside of parentheses)
splitAtFirstComma :: String -> Maybe { before :: String, after :: String }
splitAtFirstComma s = findComma s 0 0
  where
    findComma :: String -> Int -> Int -> Maybe { before :: String, after :: String }
    findComma str idx depth =
      if idx >= length str
        then Nothing
        else
          let c = take 1 (drop idx str)
          in case c of
            "(" -> findComma str (idx + 1) (depth + 1)
            ")" -> findComma str (idx + 1) (depth - 1)
            "," ->
              if depth == 0
                then Just { before: take idx str, after: drop (idx + 1) str }
                else findComma str (idx + 1) depth
            _ -> findComma str (idx + 1) depth

-- | Parse message and time from "message, time" where both may contain parens
parseMessageAndTime :: String -> Maybe { message :: String, time :: TimePoint }
parseMessageAndTime s =
  -- Find the last occurrence of a time pattern
  let
    -- Look for "night(" or "day(" from the end
    nightIdx = lastIndexOf (Pattern "night(") s
    dayIdx = lastIndexOf (Pattern "day(") s
    timeIdx = case nightIdx, dayIdx of
      Just n, Just d -> Just (max n d)
      Just n, Nothing -> Just n
      Nothing, Just d -> Just d
      Nothing, Nothing -> Nothing
  in case timeIdx of
    Just idx ->
      let
        -- Message is everything before the comma before the time
        beforeTime = take idx s
        -- Find the comma before the time
        commaIdx = lastIndexOf (Pattern ",") beforeTime
      in case commaIdx of
        Just cIdx ->
          Just { message: trim (take cIdx s)
               , time: parseTime (trim (drop (cIdx + 1) s))
               }
        Nothing -> Nothing
    Nothing -> Nothing

-- | Parse time from beginning of string, return time and remainder
parseTimeAndRest :: String -> Maybe { time :: TimePoint, rest :: String }
parseTimeAndRest s =
  let
    nightPattern = "night("
    dayPattern = "day("
  in
    if take (length nightPattern) s == nightPattern
      then findMatchingParen s (length nightPattern) 1
      else if take (length dayPattern) s == dayPattern
        then findMatchingParen s (length dayPattern) 1
        else Nothing
  where
    findMatchingParen :: String -> Int -> Int -> Maybe { time :: TimePoint, rest :: String }
    findMatchingParen str idx depth =
      if idx >= length str
        then Nothing
        else
          let c = take 1 (drop idx str)
          in case c of
            "(" -> findMatchingParen str (idx + 1) (depth + 1)
            ")" ->
              if depth == 1
                then
                  let timeStr = take (idx + 1) str
                      rest = drop (idx + 1) str
                      restTrimmed = case indexOf (Pattern ",") rest of
                        Just 0 -> drop 1 rest
                        _ -> rest
                  in Just { time: parseTime timeStr, rest: trim restTrimmed }
                else findMatchingParen str (idx + 1) (depth - 1)
            _ -> findMatchingParen str (idx + 1) depth

-- | Simple parseInt
parseInt :: String -> Maybe Int
parseInt s =
  let trimmed = trim s
  in if allDigits trimmed && trimmed /= ""
       then Just (unsafeParseInt trimmed)
       else Nothing
  where
    allDigits str = all isDigit (split (Pattern "") str)
    isDigit c = c >= "0" && c <= "9"

foreign import unsafeParseInt :: String -> Int

-- | Alternative operator for Maybe
infixl 3 alt as <|>

alt :: forall a. Maybe a -> Maybe a -> Maybe a
alt (Just x) _ = Just x
alt Nothing y = y

-- | Parse all atoms from an answer set
parseAnswerSet :: Array String -> Array Atom
parseAnswerSet = map parseAtom

-- | Parse all atoms and keep original strings
parseAnswerSetWithOriginals :: Array String -> Array ParsedAtom
parseAnswerSetWithOriginals = map \s -> { atom: parseAtom s, original: s }

-- | Build game state from parsed atoms at a specific time
buildGameState :: Array Atom -> TimePoint -> GameState
buildGameState atoms targetTime =
  let
    -- Get chairs
    chairs = mapMaybe getChair atoms

    -- Get initial assignments (time 0)
    assignments = mapMaybe getAssignment atoms

    -- Get received tokens
    tokens = mapMaybe getReceived atoms

    -- Get alive status at target time (or latest before it)
    aliveAtoms = filter (isAliveAt targetTime) atoms
    alivePlayers = mapMaybe getAliveName aliveAtoms

    -- Get dead status at target time
    deadAtoms = filter (isDeadAt targetTime) atoms
    deadPlayers = mapMaybe getDeadName deadAtoms

    -- Get ghost vote used status at target time
    ghostVoteAtoms = filter (isGhostVoteUsedAt targetTime) atoms
    ghostVoteUsedPlayers = mapMaybe getGhostVoteUsedName ghostVoteAtoms

    -- Get all reminder atoms to find placement times
    allReminders = mapMaybe getReminder atoms

    -- Get all time points in sorted order (the full timeline)
    allTimePoints = nub $ sortBy compareTimePoints $ mapMaybe getTimeFromAtom atoms

    -- Get reminders active at target time, with their placement time
    -- (start of current continuous run, not earliest ever)
    -- Sort by placement time (oldest first = closer to role token)
    remindersAtTime = sortBy (comparing _.placedAt) $
      mapMaybe (getReminderWithPlacement allReminders allTimePoints targetTime) atoms

    -- Build player list
    players = chairs # map \c ->
      let
        role = fromMaybe "?" $ lookup c.name assignments
        token = fromMaybe role $ lookup c.name tokens
        isAlive = elem c.name alivePlayers && not (elem c.name deadPlayers)
        hasUsedGhostVote = elem c.name ghostVoteUsedPlayers
      in { name: c.name, chair: c.pos, role, token, alive: isAlive, ghostVoteUsed: hasUsedGhostVote }
  in
    { players: sortBy (comparing _.chair) players
    , reminders: remindersAtTime
    , time: targetTime
    }
  where
    getChair (Chair name pos) = Just { name, pos }
    getChair _ = Nothing

    getAssignment (Assigned 0 player role) = Just { key: player, value: role }
    getAssignment _ = Nothing

    getReceived (Received player token) = Just { key: player, value: token }
    getReceived _ = Nothing

    isAliveAt t (Alive _ aliveTime) = aliveTime == t || compareTimePoints aliveTime t == LT
    isAliveAt _ _ = false

    isDeadAt t (Dead _ deadTime) = deadTime == t || compareTimePoints deadTime t == LT
    isDeadAt _ _ = false

    getAliveName (Alive name _) = Just name
    getAliveName _ = Nothing

    getDeadName (Dead name _) = Just name
    getDeadName _ = Nothing

    isGhostVoteUsedAt t (GhostVoteUsed _ voteTime) = voteTime == t || compareTimePoints voteTime t == LT
    isGhostVoteUsedAt _ _ = false

    getGhostVoteUsedName (GhostVoteUsed name _) = Just name
    getGhostVoteUsedName _ = Nothing

    -- Extract all reminder atoms with their times
    getReminder (ReminderOn token player time) = Just { token, player, time }
    getReminder _ = Nothing

    -- Extract time from various atom types
    getTimeFromAtom (Time t) = Just t
    getTimeFromAtom (StTells _ _ _ t) = Just t
    getTimeFromAtom (PlayerChooses _ _ _ t) = Just t
    getTimeFromAtom (ReminderOn _ _ t) = Just t
    getTimeFromAtom (Alive _ t) = Just t
    getTimeFromAtom (Dead _ t) = Just t
    getTimeFromAtom (GhostVoteUsed _ t) = Just t
    getTimeFromAtom (ActingRole t _) = Just t
    getTimeFromAtom _ = Nothing

    -- For a reminder active at target time, find its placement time
    -- (start of current continuous run, handling gaps from removal/re-placement)
    getReminderWithPlacement allRems timeline t (ReminderOn token player time) =
      if time == t
        then
          let
            -- Find all times this (token, player) pair appears
            tokenTimes = map _.time $ filter (\r -> r.token == token && r.player == player) allRems
            -- Find start of continuous run ending at t
            placementTime = findRunStart tokenTimes timeline t
          in Just { token, player, placedAt: placementTime }
        else Nothing
    getReminderWithPlacement _ _ _ _ = Nothing

    -- Find the start of a continuous run ending at targetTime
    -- Walk backwards through the timeline; stop when we hit a gap
    findRunStart :: Array TimePoint -> Array TimePoint -> TimePoint -> TimePoint
    findRunStart tokenTimes timeline targetT =
      let
        -- Get index of target time in timeline
        timelineArr = timeline
        targetIdx = findIndex (\t -> t == targetT) timelineArr
      in case targetIdx of
        Nothing -> targetT
        Just idx -> walkBackwards tokenTimes timelineArr idx targetT

    walkBackwards :: Array TimePoint -> Array TimePoint -> Int -> TimePoint -> TimePoint
    walkBackwards tokenTimes timeline idx currentStart =
      if idx <= 0
        then currentStart
        else
          let prevTime = fromMaybe currentStart (index timeline (idx - 1))
          in if elem prevTime tokenTimes
               then walkBackwards tokenTimes timeline (idx - 1) prevTime
               else currentStart

    lookup key arr = map _.value $ head $ filter (\x -> x.key == key) arr

-- | Extract timeline events from atoms (legacy version without sources)
extractTimeline :: Array Atom -> Array TimelineEvent
extractTimeline atoms = extractTimelineWithSources (map (\a -> { atom: a, original: "" }) atoms)

-- | Extract timeline events from parsed atoms with source strings
extractTimelineWithSources :: Array ParsedAtom -> Array TimelineEvent
extractTimelineWithSources parsedAtoms =
  let
    stTellsEvents = mapMaybe toStTellsEvent parsedAtoms
    playerChoosesEvents = mapMaybe toPlayerChoosesEvent parsedAtoms
    -- Only include TokenPlaced events for reminders that FIRST appear at each time
    -- (reminder_on represents accumulated state, not the action of placing)
    tokenPlacedEvents = filterNewTokenPlacements $ mapMaybe toTokenPlacedEvent parsedAtoms
    executionEvents = mapMaybe toExecutionEvent parsedAtoms
  in
    sortBy compareEvents (stTellsEvents <> playerChoosesEvents <> tokenPlacedEvents <> executionEvents)
  where
    toStTellsEvent { atom: StTells role player message time, original } =
      Just $ RoleAction { time, role, eventType: "st_tells", player, message, sourceAtom: original }
    toStTellsEvent _ = Nothing

    toPlayerChoosesEvent { atom: PlayerChooses role player choice time, original } =
      Just $ RoleAction { time, role, eventType: "player_chooses", player, message: choice, sourceAtom: original }
    toPlayerChoosesEvent _ = Nothing

    toTokenPlacedEvent { atom: ReminderOn token player time, original } =
      Just $ TokenPlaced { time, token, player, sourceAtom: original }
    toTokenPlacedEvent _ = Nothing

    toExecutionEvent { atom: Executed player day, original } =
      Just $ Execution { day, player, sourceAtom: original }
    toExecutionEvent _ = Nothing

    -- Filter TokenPlaced events to only keep the earliest occurrence of each token+player
    filterNewTokenPlacements :: Array TimelineEvent -> Array TimelineEvent
    filterNewTokenPlacements events =
      let
        -- Group by token+player key, keeping only the earliest time for each
        withKeys = map (\e -> case e of
          TokenPlaced r -> { key: r.token <> ":" <> r.player, time: r.time, event: e }
          _ -> { key: "", time: Night 0 0 0, event: e }) events
        -- Find earliest time for each key
        earliestTimes = foldl (\acc item ->
          case lookup item.key acc of
            Nothing -> acc <> [{ key: item.key, time: item.time }]
            Just existingTime ->
              if compareTimePoints item.time existingTime == LT
                then map (\x -> if x.key == item.key then { key: item.key, time: item.time } else x) acc
                else acc
          ) [] withKeys
        -- Filter to only keep events at their earliest time
        isEarliest item = case lookup item.key earliestTimes of
          Just t -> item.time == t
          Nothing -> false
      in
        map _.event $ filter isEarliest withKeys

    lookup key arr = map _.time $ head $ filter (\x -> x.key == key) arr

    compareEvents e1 e2 = case e1, e2 of
      RoleAction r1, RoleAction r2 -> compareTimePoints r1.time r2.time
      TokenPlaced r1, TokenPlaced r2 -> compareTimePoints r1.time r2.time
      RoleAction r1, TokenPlaced r2 -> compareTimePoints r1.time r2.time
      TokenPlaced r1, RoleAction r2 -> compareTimePoints r1.time r2.time
      Execution _, Execution _ -> EQ
      Execution e, RoleAction r -> compare (Day e.day "exec") r.time
      RoleAction r, Execution e -> compare r.time (Day e.day "exec")
      Execution e, TokenPlaced t -> compare (Day e.day "exec") t.time
      TokenPlaced t, Execution e -> compare t.time (Day e.day "exec")

-- | Get list of all unique time points
getAllTimePoints :: Array Atom -> Array TimePoint
getAllTimePoints atoms =
  nub $ sortBy compareTimePoints $ mapMaybe getTimeFromAtom atoms
  where
    getTimeFromAtom (Time t) = Just t
    getTimeFromAtom (StTells _ _ _ t) = Just t
    getTimeFromAtom (PlayerChooses _ _ _ t) = Just t
    getTimeFromAtom (ReminderOn _ _ t) = Just t
    getTimeFromAtom (Alive _ t) = Just t
    getTimeFromAtom (Dead _ t) = Just t
    getTimeFromAtom (ActingRole t _) = Just t
    getTimeFromAtom _ = Nothing

-- | Get state at a specific time point
getStateAtTime :: Array Atom -> TimePoint -> GameState
getStateAtTime = buildGameState

-- | Get the predicate name and arity for an atom (for finding rule definitions)
atomToPredicateName :: Atom -> { name :: String, arity :: Int }
atomToPredicateName = case _ of
  Assigned _ _ _         -> { name: "assigned", arity: 3 }
  Received _ _           -> { name: "received", arity: 2 }
  StTells _ _ _ _        -> { name: "st_tells", arity: 4 }
  PlayerChooses _ _ _ _  -> { name: "player_chooses", arity: 4 }
  ReminderOn _ _ _       -> { name: "reminder_on", arity: 3 }
  Alive _ _              -> { name: "alive", arity: 2 }
  Dead _ _               -> { name: "dead", arity: 2 }
  GhostVoteUsed _ _      -> { name: "ghost_vote_used", arity: 2 }
  Time _                 -> { name: "time", arity: 1 }
  ActingRole _ _         -> { name: "acting_role", arity: 2 }
  Chair _ _              -> { name: "game_chair", arity: 2 }
  Executed _ _           -> { name: "executed", arity: 2 }
  UnknownAtom s          -> { name: takeUntilParen s, arity: 0 }
  where
    takeUntilParen s =
      case indexOf (Pattern "(") s of
        Just idx -> take idx s
        Nothing -> s

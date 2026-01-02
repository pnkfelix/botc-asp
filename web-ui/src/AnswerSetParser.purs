module AnswerSetParser
  ( Atom(..)
  , TimePoint(..)
  , GameState
  , TimelineEvent(..)
  , ParsedAtom
  , PredicateCategory(..)
  , parseAnswerSet
  , parseAnswerSetWithOriginals
  , buildGameState
  , extractTimeline
  , extractTimelineWithSources
  , getStateAtTime
  , compareTimePoints
  , atomToPredicateName
  , atomCategory
  ) where

import Prelude

import Data.Array (filter, mapMaybe, sortBy, nub, head, last, findIndex, index)
import Data.Foldable (elem, all, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, trim, indexOf, lastIndexOf, length, drop, take)

-- | A parsed atom from the answer set
-- | Note: Event predicates use d_ prefix in ASP (d_st_tells, d_player_chooses, d_executed)
-- | to distinguish them from state predicates. The parser handles both with and without prefix.
data Atom
  = Assigned Int String String          -- assigned(time, player, role)
  | Received String String              -- received(player, token)
  | StTells String String String TimePoint  -- d_st_tells(role, player, message, time)
  | PlayerChooses String String String TimePoint  -- d_player_chooses(role, player, choice, time)
  | ReminderOn String String TimePoint  -- reminder_on(token, player, time)
  | Died String TimePoint               -- d_died(player, time) - player dies at this time (mechanical)
  | DeathAnnounced String TimePoint     -- d_death_announced(player, time) - death publicly announced
  | GhostVoteUsed String TimePoint      -- ghost_vote_used(player, time) - player has used their ghost vote
  | GameOver TimePoint String           -- d_game_over(time, winner) - game ends at time, winner is "good" or "evil"
  | Time TimePoint                      -- time(timepoint)
  | ActingRole TimePoint String         -- acting_role(time, role)
  | Chair String Int                    -- game_chair(player, position)
  | Executed String Int                 -- d_executed(player, day)
  | Bag String                          -- bag(role) - role is in the physical bag
  | UnknownAtom String                  -- anything we don't recognize

derive instance eqAtom :: Eq Atom

-- | Categories of predicates based on their semantic meaning
-- |
-- | - EventPredicate: Actions/effects that happen at a specific time point
-- |   These use the d_ prefix in ASP (d_st_tells, d_player_chooses, d_executed, d_died)
-- |   to distinguish them from state. The "d" is for "delta" - a change in state.
-- |
-- | - StatePredicate: Ongoing state that holds at a time point but isn't an "event"
-- |   These describe the current situation (reminder_on, ghost_vote_used)
-- |
-- | - StructuralPredicate: Defines game structure, not tied to specific moments
-- |   Setup info (assigned, received, game_chair), timeline markers (time, acting_role)
-- |
-- | - OtherPredicate: Unknown or uncategorized atoms
data PredicateCategory
  = EventPredicate      -- Deltas (d_ prefix): d_st_tells, d_player_chooses, d_executed, d_died
  | StatePredicate      -- Ongoing state: reminder_on, ghost_vote_used
  | StructuralPredicate -- Game structure: assigned, received, game_chair, time, acting_role
  | OtherPredicate      -- Unknown atoms

derive instance eqPredicateCategory :: Eq PredicateCategory

-- | Categorize an atom by its semantic meaning
atomCategory :: Atom -> PredicateCategory
atomCategory = case _ of
  -- Event predicates: discrete actions that happen at a time point
  StTells _ _ _ _       -> EventPredicate
  PlayerChooses _ _ _ _ -> EventPredicate
  Executed _ _          -> EventPredicate
  Died _ _              -> EventPredicate
  DeathAnnounced _ _    -> EventPredicate
  GameOver _ _          -> EventPredicate
  -- State predicates: ongoing state, not events
  -- Note: reminder_on(token,player,time) represents accumulated state
  -- ("reminder is on player at time"), not the action of placing it.
  -- The timeline display has separate logic to show only first placement.
  ReminderOn _ _ _      -> StatePredicate
  GhostVoteUsed _ _     -> StatePredicate
  -- Structural predicates: game setup and timeline markers
  Assigned _ _ _        -> StructuralPredicate
  Received _ _          -> StructuralPredicate
  Chair _ _             -> StructuralPredicate
  Time _                -> StructuralPredicate
  ActingRole _ _        -> StructuralPredicate
  Bag _                 -> StructuralPredicate
  -- Unknown
  UnknownAtom _         -> OtherPredicate

-- | A parsed atom with its original string representation
type ParsedAtom = { atom :: Atom, original :: String }

-- | Time points in the game
data TimePoint
  = Setup                -- Pre-game setup, before Night 1
  | Night Int Int Int    -- night(nightNum, roleOrder, substep)
  | Dawn Int             -- dawn(N) - deaths announced publicly after night N
  | Day Int String       -- day(dayNum, phase) where phase is "0" or "exec"
  | UnknownTime String

derive instance eqTimePoint :: Eq TimePoint

-- Custom Ord instance to properly interleave nights, dawn, and days:
-- Setup < Night 1 < Dawn 1 < Day 1 < Night 2 < Dawn 2 < Day 2, etc.
instance ordTimePoint :: Ord TimePoint where
  -- Setup is before everything
  compare Setup Setup = EQ
  compare Setup _ = LT
  compare _ Setup = GT
  -- Night comparisons
  compare (Night n1 r1 s1) (Night n2 r2 s2) =
    case compare n1 n2 of
      EQ -> case compare r1 r2 of
        EQ -> compare s1 s2
        other -> other
      other -> other
  -- Dawn comparisons
  compare (Dawn d1) (Dawn d2) = compare d1 d2
  -- Day comparisons
  compare (Day d1 p1) (Day d2 p2) =
    case compare d1 d2 of
      EQ -> compare p1 p2
      other -> other
  -- Night vs Dawn: Night N comes before Dawn N
  compare (Night n _r _s) (Dawn d) =
    if n <= d then LT else GT
  compare (Dawn d) (Night n _r _s) =
    if d < n then LT else GT
  -- Dawn vs Day: Dawn N comes before Day N
  compare (Dawn d) (Day n _p) =
    if d <= n then LT else GT
  compare (Day n _p) (Dawn d) =
    if n < d then LT else GT
  -- Night vs Day interleaving (Night N < Dawn N < Day N)
  compare (Night n _r _s) (Day d _p) =
    -- Night n comes before Day n (and Dawn n), but after Day (n-1)
    if n <= d then LT else GT
  compare (Day d _p) (Night n _r _s) =
    -- Day d comes after Night d and Dawn d, but before Night (d+1)
    if d < n then LT else GT
  -- UnknownTime is after everything (except Setup which is handled above)
  compare (UnknownTime s1) (UnknownTime s2) = compare s1 s2
  compare (UnknownTime _) _ = GT
  compare _ (UnknownTime _) = LT

instance showTimePoint :: Show TimePoint where
  show Setup = "setup"
  show (Night n r s) = "night(" <> show n <> "," <> show r <> "," <> show s <> ")"
  show (Dawn n) = "dawn(" <> show n <> ")"
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
      , eventType :: String  -- "d_st_tells" or "d_player_chooses"
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
  | Death
      { time :: TimePoint
      , player :: String
      , sourceAtom :: String
      }
  | GameOverEvent
      { time :: TimePoint
      , winner :: String  -- "good" or "evil"
      , sourceAtom :: String
      }

derive instance eqTimelineEvent :: Eq TimelineEvent

-- | Game state at a particular time point
type GameState =
  { players :: Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean, ghostVoteUsed :: Boolean }
  , reminders :: Array { token :: String, player :: String, placedAt :: TimePoint }
  , time :: TimePoint
  , bagTokens :: Array String  -- roles in the physical bag
  , assignedNotInBag :: Array String  -- roles assigned to players but not received (e.g., Drunk)
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
      parseDied trimmed <|>
      parseDeathAnnounced trimmed <|>
      parseGhostVoteUsed trimmed <|>
      parseGameOver trimmed <|>
      parseTimeAtom trimmed <|>
      parseActingRole trimmed <|>
      parseChair trimmed <|>
      parseExecuted trimmed <|>
      parseBag trimmed

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

-- | Parse d_st_tells(Role, Player, Message, Time)
-- | Also accepts st_tells for backward compatibility
parseStTells :: String -> Maybe Atom
parseStTells s =
  -- Try d_st_tells first (preferred), then st_tells for backward compat
  let patternD = "d_st_tells("
      pattern = "st_tells("
  in if take (length patternD) s == patternD
     then parseStTellsArgs (drop (length patternD) (take (length s - 1) s))
     else if take (length pattern) s == pattern
          then parseStTellsArgs (drop (length pattern) (take (length s - 1) s))
          else Nothing

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

-- | Parse d_player_chooses(Role, Player, Choice, Time)
-- | Also accepts player_chooses for backward compatibility
parsePlayerChooses :: String -> Maybe Atom
parsePlayerChooses s =
  -- Try d_player_chooses first (preferred), then player_chooses for backward compat
  let patternD = "d_player_chooses("
      pattern = "player_chooses("
  in if take (length patternD) s == patternD
     then parsePlayerChoosesArgs (drop (length patternD) (take (length s - 1) s))
     else if take (length pattern) s == pattern
          then parsePlayerChoosesArgs (drop (length pattern) (take (length s - 1) s))
          else Nothing

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

-- | Parse d_died(Player, Time)
-- | Also accepts died for backward compatibility
parseDied :: String -> Maybe Atom
parseDied s =
  let patternD = "d_died("
      pattern = "died("
      parseArgs rest =
        case splitAtFirstComma rest of
          Just { before: player, after: timeStr } ->
            Just $ Died (trim player) (parseTime (trim timeStr))
          Nothing -> Nothing
  in if take (length patternD) s == patternD
     then parseArgs (drop (length patternD) (take (length s - 1) s))
     else if take (length pattern) s == pattern
          then parseArgs (drop (length pattern) (take (length s - 1) s))
          else Nothing

-- | Parse d_death_announced(Player, Time)
-- | Death announced at dawn for night deaths, at exec for execution deaths
parseDeathAnnounced :: String -> Maybe Atom
parseDeathAnnounced s =
  let pattern = "d_death_announced("
      parseArgs rest =
        case splitAtFirstComma rest of
          Just { before: player, after: timeStr } ->
            Just $ DeathAnnounced (trim player) (parseTime (trim timeStr))
          Nothing -> Nothing
  in if take (length pattern) s == pattern
     then parseArgs (drop (length pattern) (take (length s - 1) s))
     else Nothing

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

-- | Parse d_game_over(Time, Winner)
parseGameOver :: String -> Maybe Atom
parseGameOver s =
  let pattern = "d_game_over("
  in if take (length pattern) s == pattern
     then
       let rest = drop (length pattern) (take (length s - 1) s)
       in case parseTimeAndRest rest of
            Just { time, rest: winner } ->
              Just $ GameOver time (trim winner)
            Nothing -> Nothing
     else Nothing

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

-- | Parse d_executed(Player, Day)
-- | Also accepts executed for backward compatibility
parseExecuted :: String -> Maybe Atom
parseExecuted s =
  -- Try d_executed first (preferred), then executed for backward compat
  let patternD = "d_executed("
      pattern = "executed("
      parseArgs rest =
        let parts = split (Pattern ",") rest
        in case parts of
          [p, d] -> Just $ Executed (trim p) (fromMaybe 0 $ parseInt (trim d))
          _ -> Nothing
  in if take (length patternD) s == patternD
     then parseArgs (drop (length patternD) (take (length s - 1) s))
     else if take (length pattern) s == pattern
          then parseArgs (drop (length pattern) (take (length s - 1) s))
          else Nothing

-- | Parse bag(Role)
parseBag :: String -> Maybe Atom
parseBag s = do
  let pattern = "bag("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)  -- Remove "bag(" and ")"
  Just $ Bag (trim rest)

-- | Parse a time string like "night(1,2,3)", "dawn(1)", or "day(1,0)"
parseTime :: String -> TimePoint
parseTime s =
  let trimmed = trim s
  in fromMaybe (UnknownTime trimmed) $
    parseNightTime trimmed <|> parseDawnTime trimmed <|> parseDayTime trimmed

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

parseDawnTime :: String -> Maybe TimePoint
parseDawnTime s = do
  let pattern = "dawn("
  _ <- if take (length pattern) s == pattern then Just unit else Nothing
  let rest = drop (length pattern) (take (length s - 1) s)  -- Remove trailing )
  Just $ Dawn (fromMaybe 1 $ parseInt (trim rest))

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

-- | Convert a TimePoint to an integer time index for comparing with assigned/3 predicate
-- Setup and Night 1 (any phase) map to 0 (initial assignment)
-- Other nights/days/dawns map to their number
timePointToAssignedTime :: TimePoint -> Int
timePointToAssignedTime Setup = 0
timePointToAssignedTime (Night 1 _ _) = 0
timePointToAssignedTime (Night n _ _) = n
timePointToAssignedTime (Dawn n) = n
timePointToAssignedTime (Day n _) = n
timePointToAssignedTime (UnknownTime _) = 0

-- | Build game state from parsed atoms at a specific time
buildGameState :: Array Atom -> TimePoint -> GameState
buildGameState atoms targetTime =
  let
    -- Get bag tokens (roles in the physical bag)
    bagTokens = mapMaybe getBagToken atoms

    -- Get chairs
    chairs = mapMaybe getChair atoms

    -- Get all assignments (initial and mid-game role changes)
    allAssignments = mapMaybe getAllAssignments atoms

    -- Convert target time to integer for comparison with assigned(N, ...)
    targetTimeInt = timePointToAssignedTime targetTime

    -- For each player, find the most recent assignment at or before targetTime
    assignments = getEffectiveAssignments allAssignments targetTimeInt

    -- Get received tokens
    tokens = mapMaybe getReceived atoms

    -- Get all death events (d_died atoms)
    -- A player is dead at time T if any d_died(player, T') exists where T' <= T
    allDeaths = mapMaybe getDeath atoms
    deadPlayersAtTime = nub $ map _.player $ filter (diedBeforeOrAt targetTime) allDeaths

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
    -- All players start alive; they're dead if a d_died event occurred at or before targetTime
    players = chairs # map \c ->
      let
        role = fromMaybe "?" $ lookup c.name assignments
        token = fromMaybe role $ lookup c.name tokens
        isAlive = not (elem c.name deadPlayersAtTime)
        hasUsedGhostVote = elem c.name ghostVoteUsedPlayers
      in { name: c.name, chair: c.pos, role, token, alive: isAlive, ghostVoteUsed: hasUsedGhostVote }

    -- Find roles that are assigned but not in the bag (e.g., Drunk)
    -- These are roles where a player's actual role differs from their received token
    -- and the role is not in the bag
    sortedPlayers = sortBy (comparing _.chair) players
    assignedNotInBag = nub $ mapMaybe (\p ->
      if not (elem p.role bagTokens) && p.role /= "?"
        then Just p.role
        else Nothing
    ) sortedPlayers
  in
    { players: sortedPlayers
    , reminders: remindersAtTime
    , time: targetTime
    , bagTokens
    , assignedNotInBag
    }
  where
    getBagToken (Bag role) = Just role
    getBagToken _ = Nothing

    getChair (Chair name pos) = Just { name, pos }
    getChair _ = Nothing

    -- Get all assignments (not just time 0)
    getAllAssignments (Assigned t player role) = Just { time: t, player, role }
    getAllAssignments _ = Nothing

    -- For each player, find the most recent assignment at or before target time
    getEffectiveAssignments :: Array { time :: Int, player :: String, role :: String } -> Int -> Array { key :: String, value :: String }
    getEffectiveAssignments allAssigns t =
      let
        -- Get unique players
        players = nub $ map _.player allAssigns
        -- For each player, find the most recent assignment at or before t
        effectiveForPlayer p =
          let validAssigns = filter (\a -> a.player == p && a.time <= t) allAssigns
              sorted = sortBy (comparing _.time) validAssigns
          in last sorted # map \a -> { key: p, value: a.role }
      in mapMaybe effectiveForPlayer players

    getReceived (Received player token) = Just { key: player, value: token }
    getReceived _ = Nothing

    -- Extract death event from Died atom
    getDeath (Died player time) = Just { player, time }
    getDeath _ = Nothing

    -- Check if death occurred at or before target time
    diedBeforeOrAt t death = death.time == t || compareTimePoints death.time t == LT

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
    getTimeFromAtom (Died _ t) = Just t
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
    -- Death events for non-execution deaths (imp kills, slayer, etc.)
    -- Execution deaths are already shown as Execution events, so filter those out
    deathEvents = mapMaybe toDeathEvent parsedAtoms
    -- Game over events
    gameOverEvents = mapMaybe toGameOverEvent parsedAtoms
  in
    sortBy compareEvents (stTellsEvents <> playerChoosesEvents <> tokenPlacedEvents <> executionEvents <> deathEvents <> gameOverEvents)
  where
    toStTellsEvent { atom: StTells role player message time, original } =
      Just $ RoleAction { time, role, eventType: "d_st_tells", player, message, sourceAtom: original }
    toStTellsEvent _ = Nothing

    toPlayerChoosesEvent { atom: PlayerChooses role player choice time, original } =
      Just $ RoleAction { time, role, eventType: "d_player_chooses", player, message: choice, sourceAtom: original }
    toPlayerChoosesEvent _ = Nothing

    toTokenPlacedEvent { atom: ReminderOn token player time, original } =
      Just $ TokenPlaced { time, token, player, sourceAtom: original }
    toTokenPlacedEvent _ = Nothing

    toExecutionEvent { atom: Executed player day, original } =
      Just $ Execution { day, player, sourceAtom: original }
    toExecutionEvent _ = Nothing

    -- Convert DeathAnnounced to Death event, but only for non-execution deaths
    -- (execution deaths occur at day(N, exec) phase and are already shown as Execution)
    -- Night deaths are announced at dawn via d_death_announced(P, dawn(N))
    toDeathEvent { atom: DeathAnnounced player time, original } =
      case time of
        Day _ "exec" -> Nothing  -- Skip execution deaths, already shown as Execution
        _ -> Just $ Death { time, player, sourceAtom: original }
    toDeathEvent _ = Nothing

    -- Convert GameOver to GameOverEvent
    toGameOverEvent { atom: GameOver time winner, original } =
      Just $ GameOverEvent { time, winner, sourceAtom: original }
    toGameOverEvent _ = Nothing

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
      Death d1, Death d2 -> compareTimePoints d1.time d2.time
      GameOverEvent g1, GameOverEvent g2 -> compareTimePoints g1.time g2.time
      RoleAction r1, TokenPlaced r2 -> compareTimePoints r1.time r2.time
      TokenPlaced r1, RoleAction r2 -> compareTimePoints r1.time r2.time
      RoleAction r, Death d -> compareTimePoints r.time d.time
      Death d, RoleAction r -> compareTimePoints d.time r.time
      TokenPlaced t, Death d -> compareTimePoints t.time d.time
      Death d, TokenPlaced t -> compareTimePoints d.time t.time
      Execution _, Execution _ -> EQ
      Execution e, RoleAction r -> compare (Day e.day "exec") r.time
      RoleAction r, Execution e -> compare r.time (Day e.day "exec")
      Execution e, TokenPlaced t -> compare (Day e.day "exec") t.time
      TokenPlaced t, Execution e -> compare t.time (Day e.day "exec")
      Execution e, Death d -> compare (Day e.day "exec") d.time
      Death d, Execution e -> compare d.time (Day e.day "exec")
      -- GameOverEvent comparisons
      GameOverEvent g, RoleAction r -> compareTimePoints g.time r.time
      RoleAction r, GameOverEvent g -> compareTimePoints r.time g.time
      GameOverEvent g, TokenPlaced t -> compareTimePoints g.time t.time
      TokenPlaced t, GameOverEvent g -> compareTimePoints t.time g.time
      GameOverEvent g, Death d -> compareTimePoints g.time d.time
      Death d, GameOverEvent g -> compareTimePoints d.time g.time
      GameOverEvent g, Execution e -> compare g.time (Day e.day "exec")
      Execution e, GameOverEvent g -> compare (Day e.day "exec") g.time

-- | Get state at a specific time point
getStateAtTime :: Array Atom -> TimePoint -> GameState
getStateAtTime = buildGameState

-- | Get the predicate name and arity for an atom (for finding rule definitions)
-- | Event predicates use d_ prefix to indicate they are deltas
atomToPredicateName :: Atom -> { name :: String, arity :: Int }
atomToPredicateName = case _ of
  Assigned _ _ _         -> { name: "assigned", arity: 3 }
  Received _ _           -> { name: "received", arity: 2 }
  StTells _ _ _ _        -> { name: "d_st_tells", arity: 4 }
  PlayerChooses _ _ _ _  -> { name: "d_player_chooses", arity: 4 }
  ReminderOn _ _ _       -> { name: "reminder_on", arity: 3 }
  Died _ _               -> { name: "d_died", arity: 2 }
  DeathAnnounced _ _     -> { name: "d_death_announced", arity: 2 }
  GhostVoteUsed _ _      -> { name: "ghost_vote_used", arity: 2 }
  GameOver _ _           -> { name: "d_game_over", arity: 2 }
  Time _                 -> { name: "time", arity: 1 }
  ActingRole _ _         -> { name: "acting_role", arity: 2 }
  Chair _ _              -> { name: "game_chair", arity: 2 }
  Executed _ _           -> { name: "d_executed", arity: 2 }
  Bag _                  -> { name: "bag", arity: 1 }
  UnknownAtom s          -> { name: takeUntilParen s, arity: 0 }
  where
    takeUntilParen s =
      case indexOf (Pattern "(") s of
        Just idx -> take idx s
        Nothing -> s

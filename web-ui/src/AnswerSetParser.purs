module AnswerSetParser
  ( Atom(..)
  , TimePoint(..)
  , GameState
  , TimelineEvent(..)
  , parseAnswerSet
  , buildGameState
  , extractTimeline
  , getStateAtTime
  , compareTimePoints
  ) where

import Prelude

import Data.Array (filter, mapMaybe, sortBy, nub, head)
import Data.Foldable (elem, all)
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
  | Time TimePoint                      -- time(timepoint)
  | ActingRole TimePoint String         -- acting_role(time, role)
  | Chair String Int                    -- chair(player, position)
  | Executed String Int                 -- executed(player, day)
  | UnknownAtom String                  -- anything we don't recognize

derive instance eqAtom :: Eq Atom

-- | Time points in the game
data TimePoint
  = Night Int Int Int    -- night(nightNum, roleOrder, substep)
  | Day Int String       -- day(dayNum, phase) where phase is "0" or "exec"
  | UnknownTime String

derive instance eqTimePoint :: Eq TimePoint
derive instance ordTimePoint :: Ord TimePoint

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
      }
  | TokenPlaced
      { time :: TimePoint
      , token :: String
      , player :: String
      }
  | Execution
      { day :: Int
      , player :: String
      }

derive instance eqTimelineEvent :: Eq TimelineEvent

-- | Game state at a particular time point
type GameState =
  { players :: Array { name :: String, chair :: Int, role :: String, token :: String, alive :: Boolean }
  , reminders :: Array { token :: String, player :: String }
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

-- | Parse chair(Player, Position)
parseChair :: String -> Maybe Atom
parseChair s = do
  let pattern = "chair("
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

    -- Get reminders at target time
    remindersAtTime = mapMaybe (getReminderAt targetTime) atoms

    -- Build player list
    players = chairs # map \c ->
      let
        role = fromMaybe "?" $ lookup c.name assignments
        token = fromMaybe role $ lookup c.name tokens
        isAlive = elem c.name alivePlayers && not (elem c.name deadPlayers)
      in { name: c.name, chair: c.pos, role, token, alive: isAlive }
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

    getReminderAt t (ReminderOn token player time) =
      if time == t || compareTimePoints time t == LT
        then Just { token, player }
        else Nothing
    getReminderAt _ _ = Nothing

    lookup key arr = map _.value $ head $ filter (\x -> x.key == key) arr

-- | Extract timeline events from atoms
extractTimeline :: Array Atom -> Array TimelineEvent
extractTimeline atoms =
  let
    stTellsEvents = mapMaybe toStTellsEvent atoms
    playerChoosesEvents = mapMaybe toPlayerChoosesEvent atoms
    executionEvents = mapMaybe toExecutionEvent atoms
  in
    sortBy compareEvents (stTellsEvents <> playerChoosesEvents <> executionEvents)
  where
    toStTellsEvent (StTells role player message time) =
      Just $ RoleAction { time, role, eventType: "st_tells", player, message }
    toStTellsEvent _ = Nothing

    toPlayerChoosesEvent (PlayerChooses role player choice time) =
      Just $ RoleAction { time, role, eventType: "player_chooses", player, message: choice }
    toPlayerChoosesEvent _ = Nothing

    toExecutionEvent (Executed player day) =
      Just $ Execution { day, player }
    toExecutionEvent _ = Nothing

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

module AspParser
  ( Predicate
  , PredicateRef(..)
  , extractPredicates
  , findReferences
  , parseProgram
  ) where

import Prelude

import Data.Array (catMaybes, filter, nub, sortBy)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim, contains)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (global, noFlags)
import Data.Either (hush)
import Data.Tuple (Tuple(..))
import Data.Ord (comparing)

-- | A predicate name with its arity (number of arguments)
type Predicate = { name :: String, arity :: Int }

-- | A reference to a predicate in the source
data PredicateRef = PredicateRef
  { predicate :: Predicate
  , lineNumber :: Int
  , lineContent :: String
  , sourceFile :: String
  }

-- | Regex to match predicate calls: name(args) or name/arity in #show
predicateCallRegex :: Maybe Regex
predicateCallRegex = hush $ regex "([a-z_][a-zA-Z0-9_]*)\\s*\\(" noFlags

-- | Regex to match #show predicate/arity declarations
showRegex :: Maybe Regex
showRegex = hush $ regex "#show\\s+([a-z_][a-zA-Z0-9_]*)\\s*/\\s*(\\d+)" noFlags

-- | Regex to match predicate definitions (at start of rule head)
headPredicateRegex :: Maybe Regex
headPredicateRegex = hush $ regex "^\\s*([a-z_][a-zA-Z0-9_]*)\\s*\\(" noFlags

-- | Extract all unique predicates from a program source
extractPredicates :: String -> Array Predicate
extractPredicates source =
  let
    lines = split (Pattern "\n") source
    predsFromLines = lines >>= extractPredicatesFromLine
    uniquePreds = nub $ map (\p -> p.name <> "/" <> show p.arity) predsFromLines
  in
    sortBy (comparing _.name) $ catMaybes $ map parsePredString uniquePreds
  where
    parsePredString s =
      case split (Pattern "/") s of
        [name, arityStr] ->
          case parseIntMaybe arityStr of
            Just arity -> Just { name, arity }
            Nothing -> Nothing
        _ -> Nothing

-- | Simple int parser
parseIntMaybe :: String -> Maybe Int
parseIntMaybe s =
  let trimmed = trim s
  in if trimmed == "" then Nothing
     else case Array.uncons (split (Pattern "") trimmed) of
       Nothing -> Nothing
       Just _ ->
         -- Basic validation: all digits
         if allDigits trimmed
           then Just (unsafeParseInt trimmed)
           else Nothing
  where
    allDigits str = Array.all isDigit (split (Pattern "") str)
    isDigit c = c >= "0" && c <= "9"

foreign import unsafeParseInt :: String -> Int

-- | Extract predicates from a single line
extractPredicatesFromLine :: String -> Array Predicate
extractPredicatesFromLine line =
  let
    -- Skip comments
    effectiveLine = case split (Pattern "%") line of
      [] -> ""
      [l] -> l
      parts -> case Array.head parts of
        Just h -> h
        Nothing -> ""

    -- Find all predicate calls
    calls = findPredicateCalls effectiveLine

    -- Find #show declarations
    shows = findShowDeclarations effectiveLine
  in
    calls <> shows

-- | Find predicate calls like pred(args)
findPredicateCalls :: String -> Array Predicate
findPredicateCalls line = case predicateCallRegex of
  Nothing -> []
  Just rx ->
    let
      -- Find all matches and extract predicate info
      parts = split (Pattern "(") line
      results = catMaybes $ map extractCallFromPart parts
    in results
  where
    extractCallFromPart part =
      let
        -- Get the word before the opening paren
        words = split (Pattern " ") (trim part)
        lastWord = Array.last words >>= \w ->
          let cleaned = cleanIdentifier w
          in if isValidIdentifier cleaned then Just cleaned else Nothing
      in case lastWord of
        Nothing -> Nothing
        Just name ->
          -- Count arity by finding matching close paren
          let arity = countArgsInLine line name
          in Just { name, arity }

-- | Clean up an identifier (remove operators etc)
cleanIdentifier :: String -> String
cleanIdentifier s =
  let
    chars = split (Pattern "") s
    validChars = Array.takeWhile isIdentifierChar chars
  in Array.fold validChars
  where
    isIdentifierChar c =
      (c >= "a" && c <= "z") ||
      (c >= "A" && c <= "Z") ||
      (c >= "0" && c <= "9") ||
      c == "_"

-- | Check if string is a valid ASP identifier
isValidIdentifier :: String -> Boolean
isValidIdentifier s =
  case Array.head (split (Pattern "") s) of
    Nothing -> false
    Just first ->
      (first >= "a" && first <= "z") || first == "_"

-- | Count arguments for a predicate in a line (simplified)
countArgsInLine :: String -> String -> Int
countArgsInLine line name =
  case split (Pattern (name <> "(")) line of
    [] -> 0
    [_] -> 0
    parts -> case Array.index parts 1 of
      Nothing -> 0
      Just afterOpen ->
        -- Count commas until close paren, accounting for nesting
        countArgs afterOpen 0 0
  where
    countArgs :: String -> Int -> Int -> Int
    countArgs s depth count =
      case Array.uncons (split (Pattern "") s) of
        Nothing -> count + 1
        Just { head: c, tail: rest } ->
          let remaining = Array.fold rest
          in case c of
            "(" -> countArgs remaining (depth + 1) count
            ")" -> if depth == 0
                   then count + 1
                   else countArgs remaining (depth - 1) count
            "," -> if depth == 0
                   then countArgs remaining depth (count + 1)
                   else countArgs remaining depth count
            _ -> countArgs remaining depth count

-- | Find #show pred/arity declarations
findShowDeclarations :: String -> Array Predicate
findShowDeclarations line =
  if not (contains (Pattern "#show") line)
  then []
  else case showRegex of
    Nothing -> []
    Just rx -> case match rx line of
      Nothing -> []
      Just matches ->
        case Array.index matches 1, Array.index matches 2 of
          Just (Just name), Just (Just arityStr) ->
            case parseIntMaybe arityStr of
              Just arity -> [{ name, arity }]
              Nothing -> []
          _, _ -> []

-- | Find all references to a predicate in a program, tagged with source file
findReferences :: String -> String -> Predicate -> Array PredicateRef
findReferences sourceFile source pred =
  let
    lines = split (Pattern "\n") source
    indexedLines = Array.mapWithIndex Tuple lines
    matchingLines = filter (matchesPredicate pred) indexedLines
  in
    map (toRef sourceFile pred) matchingLines
  where
    matchesPredicate p (Tuple _ line) =
      -- Skip pure comment lines
      let trimmed = trim line
      in if Array.take 1 (split (Pattern "") trimmed) == ["%"]
         then false
         else contains (Pattern p.name) line

    toRef file p (Tuple idx line) = PredicateRef
      { predicate: p
      , lineNumber: idx + 1
      , lineContent: line
      , sourceFile: file
      }

-- | Parse a program and return all predicates with their references
parseProgram :: Array { name :: String, content :: String } ->
                { predicates :: Array Predicate
                , references :: Predicate -> Array PredicateRef
                }
parseProgram sources =
  let
    allContent = Array.fold $ map _.content sources
    predicates = extractPredicates allContent

    findRefs pred = sources >>= \src ->
      findReferences src.name src.content pred
  in
    { predicates, references: findRefs }

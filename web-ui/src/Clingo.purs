module Clingo
  ( ClingoResult
  , ClingoWitness
  , ClingoModels
  , ClingoTime
  , ClingoError
  , SolveResult(..)
  , GroundResult(..)
  , ScriptRoles
  , IncrementalGameState
  , IncrementalPlayer
  , IncrementalReminder
  , run
  , ground
  , init
  , restart
  , resolveIncludes
  , resolveIncludesWithPath
  , extractScriptRoles
  , gameStateToIncFacts
  , runIncremental
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign, unsafeFromForeign)

-- | Result of a successful Clingo solve
type ClingoResult =
  { "Result" :: String
  , "Call" :: Array { "Witnesses" :: Array ClingoWitness }
  , "Calls" :: Int
  , "Models" :: ClingoModels
  , "Time" :: ClingoTime
  , "Warnings" :: Array String
  }

-- | A single witness (answer set)
type ClingoWitness =
  { "Value" :: Array String
  }

-- | Model statistics
type ClingoModels =
  { "More" :: String  -- "yes" or "no"
  , "Number" :: Int
  }

-- | Timing information
type ClingoTime =
  { "CPU" :: Number
  , "Model" :: Number
  , "Solve" :: Number
  , "Total" :: Number
  , "Unsat" :: Number
  }

-- | Error result from Clingo
type ClingoError =
  { "Result" :: String  -- "ERROR"
  , "Error" :: String
  }

-- | Script roles extracted from resolved ASP program
type ScriptRoles =
  { townsfolk :: Array String
  , outsiders :: Array String
  , minions :: Array String
  , demons :: Array String
  }

-- | Parse result type
data SolveResult
  = Satisfiable ClingoResult
  | Unsatisfiable ClingoResult
  | Unknown ClingoResult
  | OptimumFound ClingoResult
  | Error String

-- | Result from grounding (may succeed with output, or fail with error)
data GroundResult
  = GroundSuccess String    -- The ground program text
  | GroundError String      -- Error message (e.g., unsupported option)

-- | Foreign imports
foreign import initImpl :: String -> Effect (Promise Unit)
foreign import runImpl :: String -> Int -> Effect (Promise Foreign)
foreign import groundImpl :: String -> Effect (Promise Foreign)
foreign import restartImpl :: String -> Effect (Promise Unit)
foreign import resolveIncludesImpl :: String -> Fn1 String (Nullable String) -> String
foreign import resolveIncludesWithPathImpl :: String -> String -> Fn1 String (Nullable String) -> String
foreign import extractScriptRolesImpl :: String -> ScriptRoles
foreign import showForeignImpl :: Foreign -> String

-- | Helper to stringify a foreign value for debugging
showForeign :: Foreign -> String
showForeign = showForeignImpl

-- | Initialize clingo-wasm with the WASM URL
init :: String -> Aff Unit
init wasmUrl = toAffE (initImpl wasmUrl)

-- | Restart clingo-wasm (terminates worker and re-initializes)
-- | Use this to cancel a long-running solve
restart :: String -> Aff Unit
restart wasmUrl = toAffE (restartImpl wasmUrl)

-- | Run a Clingo program
run :: String -> Int -> Aff SolveResult
run program numModels = do
  result <- toAffE (runImpl program numModels)
  pure $ parseResult result

-- | Ground a program without solving (outputs ground program text)
-- | This tests whether clingo-wasm supports --mode=gringo
ground :: String -> Aff GroundResult
ground program = do
  result <- toAffE (groundImpl program)
  pure $ parseGroundResult result

-- | Parse the raw foreign result from grounding
-- | clingo-wasm returns JSON with various fields depending on success/error
parseGroundResult :: Foreign -> GroundResult
parseGroundResult foreign' =
  let
    raw :: { "Result" :: String }
    raw = unsafeFromForeign foreign'
  in
    case raw."Result" of
      "ERROR" ->
        let
          err :: ClingoError
          err = unsafeFromForeign foreign'
        in
          GroundError err."Error"
      -- When grounding succeeds, clingo may return SATISFIABLE with no witnesses
      -- or the output might be in a different field. We'll need to inspect what we get.
      -- For now, return the whole JSON stringified so we can see what clingo-wasm returns
      other ->
        -- Try to extract any useful output - for now just show what we got
        GroundSuccess ("Result type: " <> other <> "\nFull result: " <> showForeign foreign')

-- | Parse the raw foreign result into our ADT
parseResult :: Foreign -> SolveResult
parseResult foreign' =
  let
    raw :: { "Result" :: String }
    raw = unsafeFromForeign foreign'
  in
    case raw."Result" of
      "ERROR" ->
        let
          err :: ClingoError
          err = unsafeFromForeign foreign'
        in
          Error err."Error"
      "SATISFIABLE" ->
        Satisfiable (unsafeFromForeign foreign')
      "UNSATISFIABLE" ->
        Unsatisfiable (unsafeFromForeign foreign')
      "UNKNOWN" ->
        Unknown (unsafeFromForeign foreign')
      "OPTIMUM FOUND" ->
        OptimumFound (unsafeFromForeign foreign')
      other ->
        Error ("Unknown result type: " <> other)

-- | Resolve #include directives in a program string
-- | Takes a file resolver function that maps filenames to their content
resolveIncludes :: String -> (String -> Maybe String) -> String
resolveIncludes program resolver =
  resolveIncludesImpl program (runFn1 \filename -> toNullable (resolver filename))

-- | Resolve #include directives with path-aware resolution
-- | currentFilePath: the path of the file being processed (for relative includes)
-- | Path resolution follows Clingo's behavior:
-- | 1. First try relative to the working directory (root)
-- | 2. If not found, try relative to the directory of the including file
resolveIncludesWithPath :: String -> String -> (String -> Maybe String) -> String
resolveIncludesWithPath program currentFilePath resolver =
  resolveIncludesWithPathImpl program currentFilePath (runFn1 \filename -> toNullable (resolver filename))

-- | Extract script roles from a resolved ASP program
-- | Parses role definitions like tb_townsfolk(washerwoman; librarian; ...).
-- | Returns categorized role lists for the script
extractScriptRoles :: String -> ScriptRoles
extractScriptRoles = extractScriptRolesImpl

-- | Types for incremental validation

-- | A simplified player record for incremental validation
type IncrementalPlayer =
  { name :: String
  , chair :: Int
  , role :: String
  , token :: String
  , alive :: Boolean
  , ghostVoteUsed :: Boolean
  }

-- | A simplified reminder record for incremental validation
type IncrementalReminder =
  { token :: String
  , player :: String
  }

-- | Game state record passed to the JS FFI for incremental validation
type IncrementalGameState =
  { players :: Array IncrementalPlayer
  , reminders :: Array IncrementalReminder
  , bagTokens :: Array String
  , bluffTokens :: Array String
  , impairmentTokens :: Array String
  }

-- | Foreign imports for incremental validation
foreign import gameStateToIncFactsImpl :: IncrementalGameState -> Int -> String
foreign import runIncrementalImpl :: String -> String -> String -> String -> Effect (Promise Foreign)

-- | Convert a game state to inc_* ASP facts for incremental validation
gameStateToIncFacts :: IncrementalGameState -> Int -> String
gameStateToIncFacts = gameStateToIncFactsImpl

-- | Run incremental validation
-- | Takes resolved incremental.lp, resolved script program, state facts, and action constraint
runIncremental :: String -> String -> String -> String -> Aff SolveResult
runIncremental incProgram scriptProgram stateFacts actionConstraint = do
  result <- toAffE (runIncrementalImpl incProgram scriptProgram stateFacts actionConstraint)
  pure $ parseResult result

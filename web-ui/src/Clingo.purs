module Clingo
  ( ClingoResult
  , ClingoWitness
  , ClingoModels
  , ClingoTime
  , ClingoError
  , SolveResult(..)
  , GroundResult
  , ScriptRoles
  , run
  , ground
  , init
  , restart
  , resolveIncludes
  , resolveIncludesWithPath
  , extractScriptRoles
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

-- | Result from grounding attempt (with timeout)
type GroundResult =
  { success :: Boolean
  , resultType :: String
  , raw :: String        -- JSON stringified result or null
  , error :: String      -- Error message if failed
  }

-- | Foreign imports
foreign import initImpl :: String -> Effect (Promise Unit)
foreign import runImpl :: String -> Int -> Effect (Promise Foreign)
foreign import groundImpl :: String -> Int -> Effect (Promise Foreign)
foreign import restartImpl :: String -> Effect (Promise Unit)
foreign import resolveIncludesImpl :: String -> Fn1 String (Nullable String) -> String
foreign import resolveIncludesWithPathImpl :: String -> String -> Fn1 String (Nullable String) -> String
foreign import extractScriptRolesImpl :: String -> ScriptRoles

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

-- | Experimental: Try to ground a program using --text option
-- | Returns raw output with timeout to prevent hanging
-- | clingo-wasm hardcodes --outf=2 (JSON), so --text may conflict or produce interesting results
ground :: String -> Int -> Aff GroundResult
ground program timeoutMs = do
  result <- toAffE (groundImpl program timeoutMs)
  pure $ unsafeFromForeign result

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

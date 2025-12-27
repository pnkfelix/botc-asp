module Clingo
  ( ClingoResult
  , ClingoWitness
  , ClingoModels
  , ClingoTime
  , ClingoError
  , SolveResult(..)
  , run
  , init
  , restart
  , resolveIncludes
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe(..))
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

-- | Parse result type
data SolveResult
  = Satisfiable ClingoResult
  | Unsatisfiable ClingoResult
  | Unknown ClingoResult
  | OptimumFound ClingoResult
  | Error String

-- | Foreign imports
foreign import initImpl :: String -> Effect (Promise Unit)
foreign import runImpl :: String -> Int -> Effect (Promise Foreign)
foreign import restartImpl :: String -> Effect (Promise Unit)
foreign import resolveIncludesImpl :: String -> Fn1 String (Nullable String) -> String

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

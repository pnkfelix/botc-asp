-- | Bindings to the jsdiff library for computing text diffs
module Diff
  ( DiffLine
  , DiffStatus(..)
  , computeLineDiff
  , createPatch
  ) where

import Prelude
import Data.Function.Uncurried (Fn2, runFn2, Fn3, runFn3)

-- | Status of a line in a diff
data DiffStatus
  = Added
  | Removed
  | Unchanged

derive instance eqDiffStatus :: Eq DiffStatus

-- | A single line in a diff with its status
type DiffLine =
  { line :: String
  , status :: DiffStatus
  }

-- | Raw result from FFI (status as string)
type RawDiffLine =
  { line :: String
  , status :: String
  }

-- | Foreign imports
foreign import computeLineDiffImpl :: Fn2 String String (Array RawDiffLine)
foreign import createPatchImpl :: Fn3 String String String String

-- | Compute a line-by-line diff between two strings
-- | Returns an array of lines with their diff status
computeLineDiff :: String -> String -> Array DiffLine
computeLineDiff oldStr newStr =
  map convertLine $ runFn2 computeLineDiffImpl oldStr newStr
  where
    convertLine :: RawDiffLine -> DiffLine
    convertLine raw =
      { line: raw.line
      , status: case raw.status of
          "added" -> Added
          "removed" -> Removed
          _ -> Unchanged
      }

-- | Create a unified diff patch string (like git diff output)
createPatch :: String -> String -> String -> String
createPatch fileName oldStr newStr =
  runFn3 createPatchImpl fileName oldStr newStr

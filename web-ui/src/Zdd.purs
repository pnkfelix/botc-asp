module Zdd
  ( ZddResult
  , SeatRole
  , runZddShadow
  ) where

import Clingo (ScriptRoles)
import Effect (Effect)

-- | A single seat-role assignment for the ZDD engine
type SeatRole =
  { seat :: Int     -- 0-based seat index
  , role :: String  -- ASP-style role name (e.g. "fortune_teller")
  }

-- | Result from running the ZDD shadow solver
-- | error is "" if no error, otherwise contains the error message
type ZddResult =
  { worldCount :: Int
  , error :: String
  }

-- | Run the ZDD shadow solver.
-- | Given script roles, player count, and seat assignment from a clingo answer set,
-- | compute the ZDD world count for Night 1 information.
-- | Runs synchronously (sub-millisecond for typical game sizes).
foreign import runZddShadowImpl :: ScriptRoles -> Int -> Array SeatRole -> Effect ZddResult

-- | Run the ZDD shadow solver (wrapper for the FFI)
runZddShadow :: ScriptRoles -> Int -> Array SeatRole -> Effect ZddResult
runZddShadow = runZddShadowImpl

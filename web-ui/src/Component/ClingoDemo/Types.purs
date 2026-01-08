-- | Types and state definitions for ClingoDemo component
-- | Extracted from ClingoDemo.purs to reduce file size
module Component.ClingoDemo.Types where

import Prelude

import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..))
import AspParser as ASP
import Component.TimelineGrimoire as TG
import Halogen as H
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

-- | Child slots for embedded components
type Slots = ( timelineGrimoire :: H.Slot TG.Query TG.Output Unit )

_timelineGrimoire :: Proxy "timelineGrimoire"
_timelineGrimoire = Proxy

-- | An entry in the undo/redo stack for token drag operations
-- Stores the inst.lp content before the change and a description
type UndoEntry =
  { instLpContent :: String   -- inst.lp content before the change
  , description :: String     -- Human-readable description of the change
  }

-- | A single file's diff (comment-stripped comparison)
type FileDiff =
  { fileName :: String         -- Name of the file that changed
  , originalLines :: Array String  -- Original lines (comments stripped)
  , currentLines :: Array String   -- Current lines (comments stripped)
  }

-- | An entry in the timing history table
-- Tracks clingo run times along with what changed and model count info
type TimingEntry =
  { diffSummary :: String        -- Brief description of what changed
  , fileDiffs :: Array FileDiff  -- Detailed per-file diffs for modal
  , modelLimit :: Int            -- Max models requested (0 = all)
  , actualModelCount :: Int      -- Actual number of models found
  , moreModels :: Boolean        -- True if there were more models than returned
  , totalTime :: Number          -- Total solve time in seconds
  , solveTime :: Number          -- Just the solve time in seconds
  , runIndex :: Int              -- Sequential index of this run (for uniqueness)
  }

-- | Component state
type State =
  { files :: Map.Map String String  -- Virtual filesystem: filepath -> content
  , currentFile :: String           -- Currently selected file for editing (full path)
  , showFileDirectory :: Boolean    -- Is file directory popup visible
  , expandedDirs :: Set.Set String  -- Which directories are expanded in the tree view
  , modelLimit :: String            -- Max models to return (empty = 5, 0 = all)
  , result :: Maybe ResultDisplay
  , isLoading :: Boolean
  , isInitialized :: Boolean
  , selectedModelIndex :: Int       -- Which model to show in Timeline/Grimoire (0-indexed)
  -- Pagination for answer sets list (prevents browser crash with many models)
  , answerSetPage :: Int            -- Current page of answer sets (0-indexed)
  -- Predicate navigator state
  , showPredicateList :: Boolean
  , selectedPredicate :: Maybe ASP.Predicate
  -- Output filter expression (for filtering displayed atoms)
  , outputFilter :: String          -- Boolean expression to filter atoms
  -- Scroll notification (shown briefly when timeline click can't find the atom)
  , scrollNotification :: Maybe String
  -- Undo/redo stacks for token drag operations
  , undoStack :: Array UndoEntry
  , redoStack :: Array UndoEntry
  -- Navigate to included file dialog
  , navigateIncludeTarget :: Maybe String  -- File path user clicked on in #include
  -- Timing history table
  , timingHistory :: Array TimingEntry     -- History of clingo run times
  , nextRunIndex :: Int                    -- Counter for unique run indices
  -- Diff modal (shows detailed diff when clicking a timing table row)
  , selectedTimingEntry :: Maybe TimingEntry  -- Entry to show in diff modal
  }

-- | How to display results
data ResultDisplay
  = ResultSuccess (Array (Array String)) -- Answer sets
  | ResultUnsat
  | ResultError String

-- | Component actions
data Action
  = Initialize
  | SelectFile String           -- Switch to editing a different file (full path)
  | SetFileContent String       -- Update current file's content
  | ToggleFileDirectory         -- Show/hide file directory popup
  | ToggleDirectory String      -- Expand/collapse a directory in tree view
  | SetModelLimit String
  | SetOutputFilter String      -- Update output filter expression
  | SetPlayerCount Int          -- Update player count via slider (updates inst.lp and URL)
  | SetMinNights Int            -- Update min nights via slider (updates inst.lp and URL)
  | SetScript String            -- Update script via dropdown (updates inst.lp and URL)
  | RunClingo
  | CancelSolve
  | SelectModel Int             -- Select which model to display in Timeline/Grimoire
  | PrevAnswerSetPage
  | NextAnswerSetPage
  | TogglePredicateList
  | SelectPredicate ASP.Predicate
  | ClosePredicateModal
  | JumpToReference String Int  -- sourceFile, lineNumber
  | HandleTimelineEvent TG.Output  -- Handle timeline event clicks
  | ClearScrollNotification     -- Clear the scroll notification message
  | Undo                        -- Undo the last token drag operation
  | Redo                        -- Redo the last undone token drag operation
  | TextareaClicked             -- Check if clicked on #include directive
  | ConfirmNavigateToInclude String  -- Navigate to the included file
  | CancelNavigateToInclude     -- Cancel the navigation dialog
  | ShowTimingDiff TimingEntry  -- Show diff modal for a timing entry
  | CloseDiffModal              -- Close the diff modal
  | CopyToClipboard String      -- Copy text to clipboard
  | CopyToClipboardStopPropagation MouseEvent String  -- Copy and stop event propagation
  | NoOp  -- Used to stop event propagation

-- | Number of answer sets to display per page (prevents browser crash with many models)
answerSetPageSize :: Int
answerSetPageSize = 20

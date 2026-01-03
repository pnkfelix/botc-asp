-- | FilterExpression module for parsing and evaluating boolean filter expressions
-- |
-- | Syntax:
-- |   term       - literal substring to match (case-insensitive)
-- |   !term      - negation (also -term works)
-- |   a | b      - OR
-- |   a b        - AND (space-separated)
-- |   (expr)     - grouping
-- |
-- | Examples:
-- |   assigned           - atoms containing "assigned"
-- |   !time              - atoms NOT containing "time"
-- |   assigned | tells   - atoms containing "assigned" OR "tells"
-- |   alice chef         - atoms containing both "alice" AND "chef"
-- |   !(time | chair)    - atoms NOT containing "time" or "chair"
module FilterExpression where

import Prelude

import Data.Function.Uncurried (Fn1, Fn2, runFn1, runFn2)

-- | Filter function type (from compiled expression)
foreign import data FilterFn :: Type

-- | Compile a filter expression into a reusable filter function
-- | Returns a function that tests whether an atom matches the filter
foreign import compileFilterImpl :: Fn1 String FilterFn

-- | Filter an array of atoms using a filter expression string
foreign import filterAtomsImpl :: Fn2 String (Array String) (Array String)

-- | Validate a filter expression
-- | Returns empty string if valid, error message if invalid
foreign import validateFilterImpl :: Fn1 String String

-- | Test if an atom matches a compiled filter
foreign import testFilterImpl :: Fn2 FilterFn String Boolean

-- | Count total individual atoms (handles space-separated atoms in array elements)
foreign import countAtomsImpl :: Fn1 (Array String) Int

-- | Compile a filter expression into a reusable filter function
compileFilter :: String -> FilterFn
compileFilter = runFn1 compileFilterImpl

-- | Filter atoms using a filter expression string
filterAtoms :: String -> Array String -> Array String
filterAtoms = runFn2 filterAtomsImpl

-- | Validate a filter expression (returns empty string if valid)
validateFilter :: String -> String
validateFilter = runFn1 validateFilterImpl

-- | Check if filter expression is valid
isValidFilter :: String -> Boolean
isValidFilter expr = validateFilter expr == ""

-- | Count total individual atoms (handles space-separated atoms in array elements)
countAtoms :: Array String -> Int
countAtoms = runFn1 countAtomsImpl

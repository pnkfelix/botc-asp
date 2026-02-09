#!/usr/bin/env bash
# Populate (or refresh) the vendored PureScript dependencies from .spago/p/.
#
# This script automates the manual process described in
# vendored-purescript-deps/README.md. It:
#   1. Runs `npx spago install` to ensure .spago/p/ is populated
#   2. Copies all dependency packages into vendored-purescript-deps/
#   3. Writes a .spago-lock-hash marker for staleness detection
#
# Usage:
#   cd web-ui
#   ./scripts/vendor-purescript-deps.sh
#
# Prerequisites:
#   - node_modules/ must be installed (npm install)
#   - spago must be available (comes with npm install)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_DIR"

VENDOR_DIR="vendored-purescript-deps"
SPAGO_PKG_DIR=".spago/p"
LOCK_FILE="spago.lock"
HASH_FILE="$VENDOR_DIR/.spago-lock-hash"

# --- Validate prerequisites ---

if [ ! -d "node_modules" ]; then
  echo "Error: node_modules/ not found." >&2
  echo "Run 'npm install' first." >&2
  exit 1
fi

if ! npx --yes spago --version >/dev/null 2>&1; then
  echo "Error: spago not available." >&2
  echo "Run 'npm install' first." >&2
  exit 1
fi

if [ ! -f "$LOCK_FILE" ]; then
  echo "Error: $LOCK_FILE not found." >&2
  exit 1
fi

echo "Using spago $(npx --yes spago --version 2>/dev/null)"

# --- Ensure .spago/p/ is populated ---

echo ""
echo "Running spago install to populate $SPAGO_PKG_DIR/ ..."
npx --yes spago install

if [ ! -d "$SPAGO_PKG_DIR" ]; then
  echo "Error: $SPAGO_PKG_DIR/ was not created by spago install." >&2
  exit 1
fi

# --- Clear existing vendored packages (preserve README.md and .spago-lock-hash) ---

echo ""
echo "Clearing existing vendored packages..."
find "$VENDOR_DIR" -mindepth 1 -maxdepth 1 -type d -exec rm -rf {} +

# --- Copy packages ---

echo "Copying packages from $SPAGO_PKG_DIR/ ..."
PKG_COUNT=0
for dir in "$SPAGO_PKG_DIR"/*/; do
  if [ -d "$dir" ]; then
    pkg_name="$(basename "$dir")"
    cp -r "$dir" "$VENDOR_DIR/$pkg_name"
    PKG_COUNT=$((PKG_COUNT + 1))
  fi
done

if [ "$PKG_COUNT" -eq 0 ]; then
  echo "Warning: No packages found in $SPAGO_PKG_DIR/." >&2
  exit 1
fi

# --- Write staleness marker ---

# Use shasum -a 256 for portability (works on macOS and Linux)
LOCK_HASH="$(shasum -a 256 "$LOCK_FILE" | awk '{print $1}')"
echo "$LOCK_HASH" > "$HASH_FILE"

# --- Summary ---

echo ""
echo "Done. Vendored $PKG_COUNT packages into $VENDOR_DIR/"
echo "Lock hash: $LOCK_HASH"
echo ""
echo "Remember to commit the updated vendored-purescript-deps/ directory."

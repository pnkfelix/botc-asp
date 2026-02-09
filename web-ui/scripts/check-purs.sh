#!/usr/bin/env bash
# Type-check PureScript source files using the vendored dependencies.
#
# This script enables PureScript type checking in environments without
# network access (e.g., Claude Code Remote) by using vendored dependency
# sources instead of relying on spago to download them.
#
# Usage:
#   cd web-ui
#   ./scripts/check-purs.sh
#
# Prerequisites:
#   - node_modules/ must be installed (npm install)
#   - vendored-purescript-deps/ must be populated (see its README.md)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

cd "$PROJECT_DIR"

# Locate the purs compiler from node_modules
PURS="./node_modules/.bin/purs"
if [ ! -x "$PURS" ]; then
  echo "Error: purs compiler not found at $PURS" >&2
  echo "Run 'npm install' first." >&2
  exit 1
fi

echo "Using purs compiler: $($PURS --version)"

# Check that vendored deps exist
VENDOR_DIR="vendored-purescript-deps"
if [ ! -d "$VENDOR_DIR" ] || [ -z "$(ls -A "$VENDOR_DIR"/ 2>/dev/null | grep -v README)" ]; then
  echo "Error: No vendored dependencies found in $VENDOR_DIR/" >&2
  echo "See $VENDOR_DIR/README.md for how to populate them." >&2
  exit 1
fi

# Count vendored packages (directories only, excluding README)
PKG_COUNT=$(find "$VENDOR_DIR" -mindepth 1 -maxdepth 1 -type d | wc -l)
echo "Found $PKG_COUNT vendored dependency packages"

# Generate FFI files that are normally created by build scripts.
# BuildInfo.js provides build metadata (PR number, timestamp).
# EmbeddedPrograms.js provides embedded .lp file contents.
# Both are required for their corresponding .purs files to compile.
echo "Generating BuildInfo.js..."
node scripts/generate-build-info.js

echo "Generating EmbeddedPrograms.js..."
node scripts/embed-lp.js

# Run the PureScript compiler.
# This performs full parsing, name resolution, and type checking.
# Compiled output goes to output/ (gitignored).
echo ""
echo "Running purs compile..."
$PURS compile \
  'src/**/*.purs' \
  "$VENDOR_DIR/*/src/**/*.purs"

echo ""
echo "Type checking passed."

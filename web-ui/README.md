# BotC ASP Web UI - Clingo FFI Spike

A proof-of-concept demonstrating PureScript + Halogen integration with clingo-wasm for interactive Blood on the Clocktower game state exploration.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                     Browser                              │
│  ┌─────────────────────────────────────────────────────┐│
│  │  Halogen Component (PureScript)                     ││
│  │  - User interface                                   ││
│  │  - State management                                 ││
│  │  - Event handling                                   ││
│  └───────────────────────┬─────────────────────────────┘│
│                          │ FFI                          │
│  ┌───────────────────────▼─────────────────────────────┐│
│  │  Clingo.purs + Clingo.js                            ││
│  │  - PureScript type-safe wrapper                     ││
│  │  - Promise → Aff conversion                         ││
│  └───────────────────────┬─────────────────────────────┘│
│                          │                              │
│  ┌───────────────────────▼─────────────────────────────┐│
│  │  clingo-wasm                                        ││
│  │  - Web Worker (clingo.web.worker.js)                ││
│  │  - WebAssembly (clingo.wasm)                        ││
│  └─────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────┘
```

## Prerequisites

- Node.js 18+ (recommended: Node.js 20+)
- npm

## Setup

```bash
cd web-ui

# Install dependencies (PureScript compiler, spago, clingo-wasm, etc.)
npm install

# Build the project
npm run build

# Start the development server
npm run dev
```

Then open http://localhost:3000 in your browser.

## Project Structure

```
web-ui/
├── src/
│   ├── Main.purs           # Application entry point
│   ├── Clingo.purs         # PureScript FFI types and bindings
│   ├── Clingo.js           # JavaScript FFI implementation
│   └── Component/
│       └── ClingoDemo.purs # Halogen demo component
├── dist/
│   ├── index.html          # HTML entry point
│   ├── app.js              # Bundled application (generated)
│   ├── clingo.wasm         # Clingo WebAssembly (copied)
│   └── clingo.web.worker.js # Web Worker (copied)
├── spago.yaml              # PureScript package configuration
└── package.json            # npm configuration
```

## Available Scripts

| Script | Description |
|--------|-------------|
| `npm run build` | Full build: compile PureScript, bundle JS, copy WASM |
| `npm run build:purs` | Compile PureScript only |
| `npm run bundle` | Bundle with spago |
| `npm run dev` | Build and serve locally |
| `npm run clean` | Remove build artifacts |

## How It Works

### Clingo FFI

The `Clingo` module provides a type-safe PureScript interface to clingo-wasm:

```purescript
-- Initialize the WASM module
init :: String -> Aff Unit

-- Run an ASP program
run :: String -> Int -> Aff SolveResult

-- Result types
data SolveResult
  = Satisfiable ClingoResult
  | Unsatisfiable ClingoResult
  | Unknown ClingoResult
  | OptimumFound ClingoResult
  | Error String
```

### Example Usage

```purescript
import Clingo as Clingo

example :: Aff Unit
example = do
  -- Initialize with path to WASM file
  Clingo.init "./clingo.wasm"

  -- Run a simple ASP program
  result <- Clingo.run "a. b :- a. #show a/0. #show b/0." 1

  case result of
    Clingo.Satisfiable res ->
      -- res."Call" contains answer sets
      pure unit
    Clingo.Error err ->
      -- Handle error
      pure unit
    _ -> pure unit
```

## Demo

The included demo component allows you to:

1. Enter ASP code in a text area
2. Click "Run Clingo" to solve
3. View answer sets or error messages

The default program demonstrates a simple Blood on the Clocktower scenario:
- 3 players
- Exactly one must be the Demon
- Show all possible assignments

## Next Steps

This spike validates that the architecture works. Future development could include:

- [ ] Load actual BotC game rules from `.lp` files
- [ ] Interactive game state visualization
- [ ] Player-role assignment UI
- [ ] Night/day action simulation
- [ ] Constraint exploration ("what if player X is the Drunk?")

## Troubleshooting

### "Initializing..." never completes
- Check browser console for WASM loading errors
- Ensure `clingo.wasm` is in the `dist/` folder
- Check that your browser supports WebAssembly

### Build fails with network errors
- The PureScript compiler and spago download from GitHub
- If behind a restrictive firewall, you may need to pre-download the binaries

### CORS errors
- The WASM module requires proper CORS headers
- Use `npx serve` or another proper HTTP server, not `file://` URLs

# Blood on the Clocktower - ASP Game Explorer

> **🎮 Try it now: [pnkfx.org/botc-asp](https://pnkfx.org/botc-asp/)**

A tool for exploring and analyzing [Blood on the Clocktower](https://bloodontheclocktower.com/) games using Answer Set Programming (ASP) with a web-based interface.

## What is this?

Blood on the Clocktower is a social deduction game where players try to identify the Demon among them. This project uses **Answer Set Programming** (via [Clingo](https://potassco.org/clingo/)) to:

- Model game rules and role abilities as logical constraints
- Find all possible game states consistent with observed information
- Validate game scenarios (satisfiable vs unsatisfiable)
- Help storytellers and players explore "what if" scenarios

## Project Structure

```
botc-asp/
├── *.lp                  # Core ASP logic files
│   ├── botc.lp          # Core game mechanics (roles, alignments, timing)
│   ├── tb.lp            # Trouble Brewing script (roles and abilities)
│   ├── players.lp       # Player configuration
│   └── types.lp         # Type definitions
├── tb_tests/            # Test cases for Trouble Brewing rules
│   ├── sat_*.lp        # Satisfiable (valid) scenarios
│   └── unsat_*.lp      # Unsatisfiable (invalid) scenarios
├── nrb_games/           # No Rolls Barred game recreations
└── web-ui/              # PureScript + Halogen web interface
    └── src/
        ├── Clingo.purs  # FFI bindings to clingo-wasm
        ├── AspParser.purs # ASP source code parser
        └── Component/   # Halogen UI components
```

## Web Interface

The web UI runs Clingo entirely in the browser via WebAssembly. Features:

- Edit ASP programs directly in the browser
- Run queries and view answer sets
- Mobile-friendly predicate navigator for exploring code
- Click-to-navigate references in ASP source

**Live demo:** https://pnkfx.org/botc-asp/

## Building

### Prerequisites

- Node.js 20+
- npm

### Build Commands

```bash
cd web-ui
npm install
npm run embed-lp    # Embed .lp files into PureScript
npm run build:purs  # Compile PureScript
npm run bundle      # Bundle JavaScript
npm run copy:wasm   # Copy clingo WASM files
```

Or use the combined build:
```bash
npm run build       # Runs all build steps
```

### Development

```bash
npm run dev         # Watch mode with live reload
```

## Design Principles

1. **Declarative game rules**: Game logic is expressed as ASP constraints, not imperative code. This makes rules easy to verify against the official rulebook.

2. **Validation through tests**: Each rule behavior is tested with `sat_*.lp` (should be satisfiable) and `unsat_*.lp` (should be unsatisfiable) test cases.

3. **Browser-first**: The web UI uses clingo-wasm so no server is required. Everything runs client-side.

4. **Mobile-friendly**: UI features like the predicate navigator are designed for phone-based development and exploration.

## ASP Concepts

**Answer Set Programming** is a declarative programming paradigm where you:
- Define facts (what is known)
- Define rules (what can be derived)
- Define constraints (what cannot happen)
- Ask Clingo to find all "answer sets" - consistent models satisfying all constraints

Example from `tb.lp`:
```prolog
% Townsfolk roles in Trouble Brewing
tb_townsfolk(washerwoman; librarian; investigator; chef; empath;
             fortune_teller; undertaker; monk; ravenkeeper; virgin;
             slayer; soldier; mayor).

% The Drunk token cannot be in the bag (it's a modifier, not a role)
never_in_bag(drunk).
mistaken_identity(drunk, townsfolk).
```

## License

This project is for educational and entertainment purposes. Blood on the Clocktower is a trademark of The Pandemonium Institute.

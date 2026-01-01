# Blood on the Clocktower - ASP Game Explorer

> **🎮 Try it now: [pnkfx.org/botc-asp](https://pnkfx.org/botc-asp/)**

A tool for exploring and analyzing [Blood on the Clocktower](https://bloodontheclocktower.com/) games using Answer Set Programming (ASP) with a web-based interactive grimoire.

## What is this?

Blood on the Clocktower is a social deduction game where players try to identify the Demon among them. This project uses **Answer Set Programming** (via [Clingo](https://potassco.org/clingo/)) to:

- Model game rules and role abilities as logical constraints
- Find all possible game states consistent with observed information
- Validate game scenarios (satisfiable vs unsatisfiable)
- Help storytellers and players explore "what if" scenarios
- Visualize game states with an interactive grimoire

## Web Interface

The web UI runs Clingo entirely in the browser via WebAssembly. No server required.

**Live demo:** https://pnkfx.org/botc-asp/

### Grimoire Visualization

The centerpiece is an interactive **grimoire view** showing:
- Players arranged in a clocktower circle
- Role tokens assigned to each player
- Reminder tokens placed by role abilities
- Death shrouds and ghost vote indicators
- Color-coded alignments (blue tones for good, red tones for evil)

### Timeline Navigation

Navigate through the game timeline:
- View game state at any point (Setup, Night 1, Day 1, Night 2, etc.)
- See how roles activate and effects propagate
- Track deaths, executions, and role changes
- Watch Imp succession (starpass) unfold

### Drag-and-Drop Tokens

Before running the solver, manipulate tokens directly:
- Drag role tokens from the Bag panel to assign players
- Move reminder tokens between players
- Changes update the ASP constraints automatically
- Undo/redo support for token operations

### Multiple Models

When Clingo finds multiple valid game states:
- Browse through different answer sets
- Select which model to visualize
- Filter displayed atoms with boolean expressions
- Paginated list prevents browser crashes with many solutions

### Editor Features

- Single editor with file tabs for all `.lp` files
- Click predicate references to jump to definitions
- Click `#include` directives to navigate to included files
- Predicate navigator for mobile-friendly code browsing
- Syntax highlighting for ASP code

## Project Structure

```
botc-asp/
├── *.lp                  # Core ASP logic files
│   ├── botc.lp          # Core game mechanics (roles, timing, abilities)
│   ├── tb.lp            # Trouble Brewing script registration
│   ├── game_end.lp      # Game end conditions
│   ├── night_order.lp   # Comprehensive night order definitions
│   ├── players.lp       # Player configuration
│   ├── inst.lp          # Instance: constraints for current scenario
│   └── types.lp         # Type definitions
├── roles/               # Role definitions organized by script
│   └── tb/              # Trouble Brewing roles
│       ├── base.lp      # Role categories and registration
│       ├── townsfolk/   # Washerwoman, Empath, Chef, etc.
│       ├── outsiders/   # Drunk, Recluse, Saint, Butler
│       ├── minions/     # Poisoner, Spy, Baron, Scarlet Woman
│       └── demons/      # Imp
├── tb_tests/            # Test cases for Trouble Brewing rules
│   ├── sat_*.lp        # Satisfiable (valid) scenarios
│   └── unsat_*.lp      # Unsatisfiable (invalid) scenarios
├── nrb_games/           # No Rolls Barred game recreations
└── web-ui/              # PureScript + Halogen web interface
    └── src/
        ├── Clingo.purs           # FFI bindings to clingo-wasm
        ├── AspParser.purs        # ASP source code parser
        ├── FilterExpression.purs # Output atom filtering
        ├── TokenConstraints.purs # Drag-drop constraint generation
        └── Component/
            ├── ClingoDemo.purs       # Main UI component
            └── TimelineGrimoire.purs # Grimoire visualization
```

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

4. **Mobile-friendly**: UI features like the predicate navigator and touch-based token dragging are designed for phone-based development and exploration.

5. **Interactive exploration**: The grimoire isn't just a viewer—you can manipulate tokens and see how constraints interact before and after solving.

6. **Modular role definitions**: Each role is defined in its own file under `roles/`, making it easy to understand, test, and extend individual role mechanics.

7. **UI actions select, not create**: Drag-and-drop operations introduce constraints on what states are legal, but never introduce new states that were previously considered impossible. For roles already in the choice set (non-never-in-bag roles), the UI can directly state facts like `bag(role).` as an optimization. For special roles like Drunk that require indirect reasoning, the UI uses assertion predicates that constrain the solver.

## ASP Concepts

**Answer Set Programming** is a declarative programming paradigm where you:
- Define facts (what is known)
- Define rules (what can be derived)
- Define constraints (what cannot happen)
- Ask Clingo to find all "answer sets" - consistent models satisfying all constraints

### Predicate Conventions

This project uses naming conventions to categorize predicates:

**Event predicates** (prefixed with `d_` for "delta"):
- `d_died(Player, Time)` - Player died at this time
- `d_poisoned(Player, Time)` - Player became poisoned
- `d_executed(Player, Time)` - Player was executed

**State predicates** (derived from events):
- `alive(Player, Time)` - Player is alive at this time
- `poisoned(Player, Time)` - Player is currently poisoned
- `current_demon(Player, Time)` - Player is the current demon

**Assert predicates** (UI-driven constraints):
- `assert_drawn(Role)` - Role token must be in the bag
- `assert_received(Player, Role)` - Player must receive this role
- `assert_distrib(Roles)` - These roles must be distributed

**Structural predicates** (game setup):
- `player(Name)` - Defines a player
- `role(Name)` - Defines a role
- `good(Role)` / `evil(Role)` - Role alignments

### Example

From `roles/tb/demons/imp.lp`:
```prolog
% Imp kills a player each night
1 { imp_target(P, night(N)) : player(P), alive(P, night(N)), P != Imp } 1 :-
    assigned(Imp, imp), alive(Imp, night(N)), night(N).

% The target dies (unless protected)
d_died(P, night(N)) :-
    imp_target(P, night(N)),
    not protected(P, night(N)).

% Imp starpass: if Imp targets themselves and a minion is alive,
% the Imp dies and a minion becomes the new Imp
```

## Game Mechanics Implemented

### Trouble Brewing Roles

**Townsfolk**: Washerwoman, Librarian, Investigator, Chef, Empath, Fortune Teller, Undertaker, Monk, Ravenkeeper, Virgin, Slayer, Soldier, Mayor

**Outsiders**: Drunk, Recluse, Saint, Butler

**Minions**: Poisoner, Spy, Baron, Scarlet Woman

**Demons**: Imp (with starpass succession)

### Game Flow

- **Setup**: Role distribution, Demon/Minion information
- **Night phases**: Role abilities activate in night order
- **Day phases**: Nominations, voting, executions
- **Game end**: Demon dies unreplaced, or 2 players remain

## License

This project is for educational and entertainment purposes. Blood on the Clocktower is a trademark of The Pandemonium Institute.

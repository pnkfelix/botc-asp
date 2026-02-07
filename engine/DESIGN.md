# Engine Design

## Architecture

```
                    ┌──────────────────┐
                    │  Player Agents   │
                    │  (Human/AI/Rand) │
                    └────────┬─────────┘
                             │ decide()
                    ┌────────▼─────────┐
                    │ AspGameOrchest-  │
                    │    rator         │
                    └────────┬─────────┘
                             │ validate_*()
                    ┌────────▼─────────┐
                    │   AspValidator   │
                    │                  │
                    │  ┌────────────┐  │
                    │  │  clingo    │  │
                    │  │            │  │
                    │  │ botc.lp    │  │
                    │  │ tb.lp      │  │
                    │  │ roles/*.lp │  │
                    │  └────────────┘  │
                    └──────────────────┘
```

## Core Principle

**All rule validation happens in ASP, not Python.**

The Python layer only:
- Orchestrates game flow (turn order, phase transitions)
- Manages agent interactions (wake, ask, receive)
- Converts state to/from ASP format

The ASP layer:
- Validates all actions (is this kill legal? is this protection valid?)
- Computes derived state (who is alive? what reminder tokens exist?)
- Enforces game rules (role distribution, ability constraints)

## Key Components

### AspValidator (`asp_validator.py`)

Validates single transitions by:
1. Converting `GameStateSnapshot` to ASP constraints
2. Adding proposed action as a constraint
3. Checking SAT/UNSAT with clingo

```python
# State is converted to constraints like:
:- not assigned(0, diana, imp).
:- not assigned(0, tom, monk).
needs_night(2).
executed(jon, 2).  # If someone was executed

# Proposed action is added as constraint:
:- not player_chooses(imp, diana, point(alice), night(2, 4, 2)).

# If SAT: action is valid
# If UNSAT: action violates game rules
```

### AspGameOrchestrator (`asp_game.py`)

Runs games using AspValidator for all ability use:

```python
result = await self._validate_and_execute_ability(role, player, ability, target)
if result.valid:
    # Apply the effect
else:
    # Handle invalid action (re-prompt, error, etc.)
```

### Agent Abstraction (`agent.py`)

Players implement `PlayerAgent`:
- `observe(view, message)` - receive information
- `decide(view, request)` - make a decision

Available implementations:
- `RandomPlayerAgent` - random choices
- `CLIPlayerAgent` - human input

### Game State (`types.py`)

Script-agnostic types:
- `GameConfig` - player names, script
- `Night`, `Day` - time phases
- `Transition` - proposed actions

### Trace Recording (`trace.py`)

Records all interactions:
- ST -> Player communications
- Player -> ST communications
- Public announcements
- Nominations and votes

Outputs: Mermaid, PlantUML, ASCII

## File Organization

```
engine/
├── README.md           # Quick start, entry points
├── DESIGN.md           # This file
├── CLAUDE.md           # AI-specific notes
├── __init__.py         # Package exports
├── types.py            # Core type definitions
├── agent.py            # Player/ST agent abstraction
├── asp_validator.py    # ASP validation (KEY FILE)
├── asp_game.py         # ASP-validated game loop
├── validator.py        # Setup enumeration
├── game.py             # Original game loop (deprecated)
├── trace.py            # Game trace recording
├── demo_game.py        # Demo entry point
├── test_*.py           # Tests
└── debug_*.py          # Debug utilities
```

## Validation Timing

ASP validation time depends on:
1. Number of players (grounding size)
2. Number of nights modeled (state space)
3. Script complexity (number of roles)

Measured times for Trouble Brewing, 9 players:

| Night | Time | Notes |
|-------|------|-------|
| 2 | ~1300ms | Minimal state |
| 3 | ~1850ms | One execution |
| 4 | ~2400ms | Two executions |

For 5 players, times are significantly faster (~300-400ms).

## Known Limitations

1. **Mid-game role changes**: Scarlet Woman becoming Imp after demon execution
   isn't tracked in simple state snapshots. The full ASP trace handles this
   via `role_assignment_changes_to`.

2. **Full trace modeling**: ASP models the entire game trace, not just the
   current state. This is correct for complex interactions but adds overhead.

3. **Information roles**: Empath, Fortune Teller, etc. aren't validated yet.
   They require ST discretion for what info to provide.

## Future Work

1. **Incremental validation**: Model only the current transition, not full trace
2. **TypeScript port**: Use clingo-wasm for browser deployment
3. **AI agents**: Implement players that use SAT enumeration to reason
4. **Multi-agent training**: Self-play for learning

# BotC Engine

A Python framework for running Blood on the Clocktower games with ASP-validated rules.

## Quick Start

```bash
cd botc-asp

# Run a random game with ASP validation
python3 -m engine.demo_game

# Validate transitions from a real NRB game
python3 engine/test_nrb_transitions.py

# Quick validation demo
python3 engine/test_nrb_transitions.py --quick

# Run the ASP validator demo
python3 engine/asp_validator.py

# Run tests
python3 engine/test_single_transition.py
python3 engine/test_real_asp.py
```

## Entry Points

### For Playing Games

| Command | Description |
|---------|-------------|
| `python3 -m engine.demo_game` | Run a full random game with trace output |
| `python3 engine/asp_game.py` | Run ASP-validated game (currently uses random agents) |

### For Testing/Development

| Command | Description |
|---------|-------------|
| `python3 engine/test_nrb_transitions.py` | Validate 7 transitions from NRB Episode 001 |
| `python3 engine/test_nrb_transitions.py --quick` | Quick single-transition demo |
| `python3 engine/test_real_asp.py` | Test ASP validation with various scenarios |
| `python3 engine/test_single_transition.py` | Minimal ASP validation timing test |
| `python3 engine/asp_validator.py` | Demo the AspValidator class |
| `python3 engine/debug_nrb.py` | Debug NRB validation issues |

### For Library Use

```python
from engine import AspValidator, GameStateSnapshot, run_asp_game
from pathlib import Path

# Validate a single action
validator = AspValidator(Path("."), script="tb")
state = GameStateSnapshot(
    players=frozenset(["alice", "bob", "diana"]),
    seating={"alice": 0, "bob": 1, "diana": 2},
    assignments={"alice": "monk", "bob": "empath", "diana": "imp"},
    received={},  # Let ASP derive
    bag=frozenset(),  # Let ASP derive
    bluffs=frozenset(),  # Let ASP derive
    current_night=2,
    executions={},
)

result = validator.validate_imp_kill(state, "diana", "alice")
print(f"Valid: {result.valid}, Time: {result.elapsed_ms:.0f}ms")
```

## Performance

Validation times (9 players, Trouble Brewing):

| Night | Validation Time |
|-------|-----------------|
| Night 2 | ~1300ms |
| Night 3 | ~1850ms |
| Night 4 | ~2400ms |

Times increase with the number of nights being modeled because ASP models the full game trace.

## TypeScript Portability

The same ASP validation approach works in TypeScript/browser using clingo-wasm.
See [../engine-ts/README.md](../engine-ts/README.md) for the proof-of-concept.

```bash
cd engine-ts
npm install
npx tsc
node dist/validate.js
```

Performance comparison:

| Environment | 5 Players | 9P Night 2 | 9P Night 3 | 9P Night 4 |
|-------------|-----------|------------|------------|------------|
| Python (clingo) | ~300-400ms | ~1300ms | ~1850ms | ~2400ms |
| TypeScript (clingo-wasm) | ~520-630ms | ~1600ms | ~2300ms | ~2950ms |

clingo-wasm is ~1.2-1.5x slower than native clingo - acceptable for turn-based games.

## Files

See [DESIGN.md](DESIGN.md) for architecture details.

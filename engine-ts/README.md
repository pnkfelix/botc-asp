# BotC Engine - TypeScript Implementation

TypeScript implementation of the BotC ASP validator with **full feature parity**
with the Python implementation, including both full-trace and incremental validation modes.

## Features

- **Full-trace mode**: Validates complete game history, O(n) with game length
- **Incremental mode**: Validates single transitions in O(1) time (~40-80ms)
- **40-60x speedup** with incremental mode vs full-trace
- Works in Node.js and browser environments via [clingo-wasm](https://github.com/domoritz/clingo-wasm)

## Quick Start

```bash
cd engine-ts
npm install
npx tsc
node dist/validate.js
```

## Architecture

```
engine-ts/
├── types.ts          # Core types (GameStateSnapshot, ValidationMode, etc.)
├── asp-validator.ts  # AspValidator class with both validation modes
└── validate.ts       # Test runner comparing both modes
```

## API Usage

```typescript
import { AspValidator } from './asp-validator';
import { ValidationMode, createGameState } from './types';

// Create a game state
const state = createGameState({
  players: ['alice', 'bob', 'charlie', 'diana', 'eve'],
  seating: { alice: 0, bob: 1, charlie: 2, diana: 3, eve: 4 },
  assignments: {
    alice: 'washerwoman',
    bob: 'empath',
    charlie: 'monk',
    diana: 'imp',
    eve: 'poisoner',
  },
  currentNight: 2,
  executions: {},
});

// Use incremental mode (fast, O(1))
const validator = new AspValidator('/path/to/botc-asp', 'tb', ValidationMode.INCREMENTAL);

// Validate an Imp kill
const result = await validator.validateImpKill(state, 'diana', 'alice');
console.log(result.valid);      // true
console.log(result.elapsedMs);  // ~40ms

// Validate starpass (self-kill)
const starpass = await validator.validateImpKill(state, 'diana', 'diana');
console.log(starpass.valid);    // true (minion eve is alive)
```

## Performance Comparison

| Test | Full-Trace | Incremental | Speedup |
|------|-----------|-------------|---------|
| 5P Night 2: Imp kill | ~600ms | ~40ms | 15x |
| 5P Night 2: Starpass | ~520ms | ~35ms | 15x |
| 9P Night 2: Imp kill | ~1600ms | ~45ms | 35x |
| 9P Night 3: Imp kill | ~2300ms | ~50ms | 46x |
| 9P Night 4: Imp kill | ~2950ms | ~55ms | 54x |

clingo-wasm is ~1.2-1.5x slower than native Python clingo, but incremental mode
still provides massive speedups that make real-time validation practical.

## Validation Modes

### Full-Trace Mode
- Uses `botc.lp` with inertia rules
- Derives all state from initial conditions
- Validates complete game history consistency
- Time grows with game length

### Incremental Mode
- Uses `incremental.lp` with state input predicates
- Accepts current state as `inc_*` facts
- Validates single action in isolation
- Constant time regardless of game length

## Key Differences from Python

1. **No `#include` support**: clingo-wasm doesn't support `#include` directives,
   so we inline them using `resolveIncludes()`.

2. **Async API**: `clingo.run()` returns a Promise, not a synchronous result.

3. **JSON output**: Results come back as JSON with `Result`, `Call`, `Witnesses`, etc.

## Browser Usage

```html
<script src="https://cdn.jsdelivr.net/npm/clingo-wasm@latest"></script>
<script type="module">
  import { AspValidator } from './asp-validator.js';
  import { ValidationMode, createGameState } from './types.js';

  // Note: You'll need to bundle the ASP files or fetch them
  const validator = new AspValidator(aspPath, 'tb', ValidationMode.INCREMENTAL);
  const result = await validator.validateImpKill(state, 'diana', 'alice');
</script>
```

## Files

- `types.ts` - Type definitions matching Python `types.py`
- `asp-validator.ts` - `AspValidator` class matching Python `asp_validator.py`
- `validate.ts` - Comparison test suite for both modes

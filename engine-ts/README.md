# BotC Engine - TypeScript Proof of Concept

This demonstrates that the ASP validation approach works in TypeScript/browser
environments using [clingo-wasm](https://github.com/domoritz/clingo-wasm).

## Quick Start

```bash
cd engine-ts
npm install
npx tsc
node dist/validate.js
```

## Output

```
============================================================
TypeScript/clingo-wasm Validation Proof-of-Concept
============================================================

--- 5-Player Tests ---

Test 1: diana (Imp) kills alice [5 players, Night 2]
  Valid: true
  Time: 635ms

Test 2: diana (Imp) starpass [5 players, Night 2]
  Valid: true
  Time: 519ms

--- 9-Player NRB Tests (Episode 001) ---

Test 3: luke (Imp) kills sullivan [9 players, Night 2]
  Valid: true
  Time: 1594ms

Test 4: luke (Imp) kills tom [9 players, Night 3, after Day 2 execution]
  Valid: true
  Time: 2304ms

Test 5: luke (Imp) kills oli [9 players, Night 4, after Day 2+3 executions]
  Valid: true
  Time: 2944ms

============================================================
Summary
============================================================

5-Player Results:
  Test 1 (Imp kill):  PASS - 635ms
  Test 2 (Starpass):  PASS - 519ms

9-Player NRB Results:
  Test 3 (Night 2):   PASS - 1594ms
  Test 4 (Night 3):   PASS - 2304ms
  Test 5 (Night 4):   PASS - 2944ms

TypeScript portability: CONFIRMED
The same ASP validation approach works in Node.js/browser.
```

## Key Differences from Python

1. **No `#include` support**: clingo-wasm doesn't support `#include` directives,
   so we inline them using `resolveIncludes()`.

2. **Async API**: `clingo.run()` returns a Promise, not a synchronous result.

3. **JSON output**: Results come back as JSON with `Result`, `Call`, `Witnesses`, etc.

## Performance

| Environment | 5 Players | 9P Night 2 | 9P Night 3 | 9P Night 4 |
|-------------|-----------|------------|------------|------------|
| Python (clingo) | ~300-400ms | ~1300ms | ~1850ms | ~2400ms |
| TypeScript (clingo-wasm) | ~520-630ms | ~1600ms | ~2300ms | ~2950ms |

clingo-wasm is ~1.2-1.5x slower than native clingo, which is acceptable for
turn-based gameplay.

## Browser Usage

```html
<script src="https://cdn.jsdelivr.net/npm/clingo-wasm@latest"></script>
<script>
  async function validate() {
    const program = `... your ASP program ...`;
    const result = await clingo.run(program, 1);
    console.log(result.Result); // 'SATISFIABLE' or 'UNSATISFIABLE'
  }
  validate();
</script>
```

## Next Steps

1. Port `AspValidator` class to TypeScript
2. Port `GameStateSnapshot` type definitions
3. Create browser-compatible file bundler for ASP includes

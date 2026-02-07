# Notes for Claude

AI-specific notes for working with this codebase.

## Key Insight

**Don't duplicate ASP logic in Python.**

The original `game.py` had hardcoded role logic like:
```python
if role == "imp":
    targets = [p for p in alive if p != player_name]
elif role == "poisoner":
    # etc
```

This is WRONG. All validation should go through ASP.

## Correct Approach

Use `AspValidator` for all ability validation:
```python
result = validator.validate_imp_kill(state, "diana", "alice")
if result.valid:
    # Apply effect
```

The validator works by:
1. Encoding state as ASP constraints
2. Adding proposed action as constraint
3. Checking SAT/UNSAT

## State Encoding

**Minimal constraints work best.** The validator only needs:
- Player count and seating
- Role assignments
- Current night number
- Executions (for death tracking)

Do NOT constrain `received`, `bag`, `bluffs` - let ASP derive these from assignments to avoid over-constraining.

## Time Model

ASP uses structured time: `night(N, RoleOrd, Substep)`
- N = night number
- RoleOrd = role's position in night order (from base.lp)
- Substep = 1=ST asks, 2=player chooses, 3=ST places token

Role orders for Trouble Brewing (`roles/tb/base.lp`):
```
other_night_role_order(poisoner, 1).
other_night_role_order(monk, 2).
other_night_role_order(scarlet_woman, 3).
other_night_role_order(imp, 4).
other_night_role_order(ravenkeeper, 5).
...
```

## Testing Approach

1. **Minimal tests first**: `test_single_transition.py` proves ASP validation is fast enough
2. **Real data tests**: `test_nrb_transitions.py` validates against actual game traces
3. **Debug scripts**: `debug_nrb.py` helps diagnose UNSAT issues

## Common Issues

### UNSAT when it should be SAT

Usually means over-constraining. Check:
- Are you constraining `received` inconsistently with `assigned`?
- Are you constraining `bag` with roles not matching the distribution?
- Did you forget to add `needs_night(N)` for nights > 1?

### Slow validation

Validation time grows with:
- Player count (more grounding)
- Night count (more time points)
- Script complexity (more roles)

For interactive use, keep state minimal.

## Files to Understand

Priority order:
1. `asp_validator.py` - The core validation interface
2. `../botc.lp` - Core ASP infrastructure
3. `../roles/tb/base.lp` - Role definitions and night order
4. `../roles/tb/demons/imp.lp` - Example role implementation
5. `types.py` - Python type definitions

## Running Tests

```bash
cd botc-asp

# Fast sanity check
python3 engine/test_single_transition.py

# Full NRB validation
python3 engine/test_nrb_transitions.py

# Debug a specific issue
python3 engine/debug_nrb.py
```

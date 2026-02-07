#!/usr/bin/env python3
"""
Test incremental validation mode vs full-trace mode.

This compares:
1. Full-trace mode (botc.lp): derives state via inertia, models all nights
2. Incremental mode (incremental.lp): accepts state as input, validates one action

Both should give the same validation results, but incremental should be MUCH faster
and have constant time regardless of which night we're on.
"""

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import clingo


def validate_full_trace(
    asp_path: Path,
    night: int,
    imp_player: str,
    target: str,
    executions: dict[int, str] = None,
) -> tuple[bool, float]:
    """Validate using full-trace mode (botc.lp + tb.lp)."""
    executions = executions or {}

    execution_facts = "\n".join(
        f"executed({player}, {day})." for day, player in executions.items()
    )

    program = f"""
#const player_count = 9.

name(luke). chair(luke, 0).
name(oli). chair(oli, 1).
name(blair). chair(blair, 2).
name(tom). chair(tom, 3).
name(elliott). chair(elliott, 4).
name(laurie). chair(laurie, 5).
name(isaac). chair(isaac, 6).
name(jon). chair(jon, 7).
name(sullivan). chair(sullivan, 8).

needs_night({night}).

:- not assigned(0, luke, imp).
:- not assigned(0, oli, ravenkeeper).
:- not assigned(0, blair, fortune_teller).
:- not assigned(0, tom, monk).
:- not assigned(0, elliott, recluse).
:- not assigned(0, laurie, scarlet_woman).
:- not assigned(0, isaac, undertaker).
:- not assigned(0, jon, chef).
:- not assigned(0, sullivan, drunk).

{execution_facts}

:- not player_chooses(imp, {imp_player}, point({target}), night({night}, 4, 2)).

#show player_chooses/4.
"""

    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))
    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(True))

    elapsed = (time.perf_counter() - start) * 1000
    return bool(result), elapsed


def validate_incremental(
    asp_path: Path,
    night: int,
    imp_player: str,
    target: str,
    alive_players: list[str],
    role_assignments: dict[str, str],
    minion_alive: bool = True,
) -> tuple[bool, float]:
    """Validate using incremental mode (incremental.lp + tb.lp)."""

    # Build state facts
    alive_facts = "\n".join(f"inc_alive({p})." for p in alive_players)
    role_facts = "\n".join(f"inc_role({p}, {r})." for p, r in role_assignments.items())

    minion_fact = "inc_minion_alive." if minion_alive else ""

    program = f"""
#const player_count = 9.

name(luke). chair(luke, 0).
name(oli). chair(oli, 1).
name(blair). chair(blair, 2).
name(tom). chair(tom, 3).
name(elliott). chair(elliott, 4).
name(laurie). chair(laurie, 5).
name(isaac). chair(isaac, 6).
name(jon). chair(jon, 7).
name(sullivan). chair(sullivan, 8).

current_night({night}).

% Current state (provided as facts)
{alive_facts}

{role_facts}

{minion_fact}

% Proposed action
:- not player_chooses(imp, {imp_player}, point({target}), night({night}, 4, 2)).

#show player_chooses/4.
"""

    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "incremental.lp"))
    ctl.load(str(asp_path / "tb.lp"))
    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(True))

    elapsed = (time.perf_counter() - start) * 1000
    return bool(result), elapsed


def main():
    asp_path = Path(__file__).parent.parent

    print("=" * 70)
    print("Incremental vs Full-Trace Validation Comparison")
    print("=" * 70)
    print()

    # NRB Episode 001 role assignments
    roles = {
        "luke": "imp",
        "oli": "ravenkeeper",
        "blair": "fortune_teller",
        "tom": "monk",
        "elliott": "recluse",
        "laurie": "scarlet_woman",
        "isaac": "undertaker",
        "jon": "chef",
        "sullivan": "drunk",
    }

    tests = [
        # (night, imp, target, executions_before, alive_at_night)
        (2, "luke", "sullivan", {},
         ["luke", "oli", "blair", "tom", "elliott", "laurie", "isaac", "jon", "sullivan"]),
        (3, "luke", "tom", {2: "jon"},
         ["luke", "oli", "blair", "tom", "elliott", "laurie", "isaac", "sullivan"]),
        (4, "luke", "oli", {2: "jon", 3: "elliott"},
         ["luke", "oli", "blair", "laurie", "isaac", "sullivan"]),  # tom died night 3
    ]

    print(f"{'Test':<40} {'Full-Trace':<15} {'Incremental':<15} {'Speedup':<10}")
    print("-" * 80)

    for night, imp, target, execs, alive in tests:
        # Full-trace mode
        valid_ft, time_ft = validate_full_trace(asp_path, night, imp, target, execs)

        # Incremental mode
        # Determine if minion is alive
        minion_alive = any(roles[p] in ["poisoner", "spy", "scarlet_woman", "baron"]
                          for p in alive if p in roles)

        valid_inc, time_inc = validate_incremental(
            asp_path, night, imp, target, alive, roles, minion_alive
        )

        # Check results match
        match = "✓" if valid_ft == valid_inc else "✗ MISMATCH"
        speedup = time_ft / time_inc if time_inc > 0 else float('inf')

        test_name = f"Night {night}: {imp} kills {target}"
        print(f"{test_name:<40} {time_ft:>10.0f}ms   {time_inc:>10.0f}ms   {speedup:>6.1f}x {match}")

    print()
    print("=" * 70)
    print("Starpass Test (Imp self-kill)")
    print("=" * 70)
    print()

    # Test starpass with minion alive (should be VALID)
    valid_ft, time_ft = validate_full_trace(asp_path, 2, "luke", "luke", {})
    valid_inc, time_inc = validate_incremental(
        asp_path, 2, "luke", "luke",
        ["luke", "oli", "blair", "tom", "elliott", "laurie", "isaac", "jon", "sullivan"],
        roles, minion_alive=True
    )
    match = "✓" if valid_ft == valid_inc else "✗ MISMATCH"
    speedup = time_ft / time_inc if time_inc > 0 else float('inf')
    print(f"{'Starpass (minion alive)':<40} {time_ft:>10.0f}ms   {time_inc:>10.0f}ms   {speedup:>6.1f}x {match}")
    print(f"  Expected: VALID, Got: FT={valid_ft}, INC={valid_inc}")

    # Test starpass with no minion alive (should be INVALID)
    # Simulate: laurie (scarlet_woman) was executed
    valid_ft, time_ft = validate_full_trace(asp_path, 3, "luke", "luke", {2: "laurie"})
    valid_inc, time_inc = validate_incremental(
        asp_path, 3, "luke", "luke",
        ["luke", "oli", "blair", "tom", "elliott", "isaac", "jon", "sullivan"],
        roles, minion_alive=False
    )
    match = "✓" if valid_ft == valid_inc else "✗ MISMATCH"
    speedup = time_ft / time_inc if time_inc > 0 else float('inf')
    print(f"{'Starpass (no minion)':<40} {time_ft:>10.0f}ms   {time_inc:>10.0f}ms   {speedup:>6.1f}x {match}")
    print(f"  Expected: INVALID, Got: FT={valid_ft}, INC={valid_inc}")

    print()
    print("=" * 70)
    print("Summary")
    print("=" * 70)
    print()
    print("Incremental mode validates the SAME constraints as full-trace mode,")
    print("but only models the current night instead of deriving full history.")
    print("This gives constant-time validation regardless of which night it is.")


if __name__ == "__main__":
    main()

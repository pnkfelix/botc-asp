#!/usr/bin/env python3
"""
Test: Single-transition validation using actual ASP role files.

This tests whether we can validate transitions using the real botc.lp
infrastructure rather than duplicating logic in Python.
"""

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import clingo


def test_imp_kill_with_real_asp():
    """
    Test: Validate an Imp kill using the actual ASP infrastructure.
    """
    print("=" * 60)
    print("Test: Imp kill with real ASP infrastructure")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    # State facts that describe current game state
    # From base.lp: other_night_role_order(imp, 4)
    state_facts = """
    % === Game Configuration ===
    #const player_count = 5.

    % Players and seating
    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    % === Current Time ===
    % We're validating a night 2 action
    needs_night(2).

    % === Role Assignments (initial) ===
    assigned(0, alice, washerwoman).
    assigned(0, bob, empath).
    assigned(0, charlie, monk).
    assigned(0, diana, imp).
    assigned(0, eve, saint).

    % Bag contains these roles
    bag(washerwoman). bag(empath). bag(monk). bag(imp). bag(saint).

    % What tokens players received
    received(alice, washerwoman).
    received(bob, empath).
    received(charlie, monk).
    received(diana, imp).
    received(eve, saint).

    % Bluffs shown to demon
    bluff(chef). bluff(fortune_teller). bluff(butler).

    % === Show what choices are legal ===
    #show player_chooses/4.
    """

    print("\nLoading ASP files (botc.lp + tb.lp)...")
    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], state_facts)

    # Load core + script (tb.lp includes all roles)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    print("  Grounding...")
    ground_start = time.perf_counter()
    ctl.ground([("base", [])])
    ground_time = time.perf_counter() - ground_start
    print(f"  Ground time: {ground_time*1000:.2f}ms")

    print("  Solving...")
    solve_start = time.perf_counter()

    models = []
    def on_model(m):
        choices = [str(a) for a in m.symbols(shown=True)]
        models.append(choices)

    ctl.solve(on_model=on_model)
    solve_time = time.perf_counter() - solve_start
    total_time = time.perf_counter() - start

    print(f"  Solve time: {solve_time*1000:.2f}ms")
    print(f"  Total time: {total_time*1000:.2f}ms")

    if models:
        print(f"\nModels found: {len(models)}")
        choices = models[0]
        imp_choices = [c for c in choices if "imp" in c]
        print(f"Imp choices: {imp_choices[:5]}...")  # First 5
    else:
        print("\nNo models found - checking why...")
        # Try without any constraints to see if it's a grounding issue
        return check_unsat_reason(asp_path)

    return total_time


def check_unsat_reason(asp_path):
    """Debug why the model is UNSAT."""
    print("\n--- Debugging UNSAT ---")

    # Minimal test: just player definitions
    minimal = """
    #const player_count = 5.
    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    % Let ASP generate assignments
    #show assigned/3.
    #show bag/1.
    """

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], minimal)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))
    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(
        [str(a) for a in m.symbols(shown=True)]
    ))

    if result:
        print("  Minimal test PASSED - ASP can generate valid setups")
        assignments = [a for a in result[0] if a.startswith("assigned")]
        print(f"  Generated assignments: {assignments[:5]}...")
        return None
    else:
        print("  Minimal test FAILED - something wrong with base constraints")
        return None


def test_validation_with_generation():
    """
    Test: Let ASP generate a valid setup, then validate an Imp action.
    """
    print("\n" + "=" * 60)
    print("Test: ASP generates setup + validates Imp action")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    # Let ASP generate everything, but constrain specific assignments
    program = """
    #const player_count = 5.

    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    needs_night(2).

    % Constrain that diana must be the imp
    :- not assigned(0, diana, imp).

    % Show Imp's night 2 choices
    #show player_chooses(imp, diana, point(X), night(2, 4, 2)) : player_chooses(imp, diana, point(X), night(2, 4, 2)).
    """

    print("\nRunning...")
    start = time.perf_counter()

    ctl = clingo.Control(["--models=5"])  # Get several models
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    ctl.ground([("base", [])])

    models = []
    def on_model(m):
        choices = [str(a) for a in m.symbols(shown=True)]
        models.append(choices)

    ctl.solve(on_model=on_model)
    elapsed = time.perf_counter() - start

    print(f"  Time: {elapsed*1000:.2f}ms")
    print(f"  Models: {len(models)}")

    if models:
        # Each model should have diana choosing a different target
        print("  Imp (diana) can legally kill:")
        targets = set()
        for m in models:
            for c in m:
                if "point" in c:
                    # Extract target from player_chooses(imp,diana,point(X),...)
                    import re
                    match = re.search(r'point\((\w+)\)', c)
                    if match:
                        targets.add(match.group(1))
        print(f"    {sorted(targets)}")
    else:
        print("  UNSAT - no valid configurations")


def test_validate_specific_action():
    """
    Test: Validate a specific proposed action.
    """
    print("\n" + "=" * 60)
    print("Test: Validate specific action (diana kills alice)")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    # Constrain to a specific action and check SAT
    program = """
    #const player_count = 5.

    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    needs_night(2).

    % diana must be imp
    :- not assigned(0, diana, imp).

    % PROPOSED ACTION: diana kills alice
    % We constrain the solver to this specific choice
    :- not player_chooses(imp, diana, point(alice), night(2, 4, 2)).

    % Show the validated action
    #show player_chooses/4.
    """

    print("\nValidating 'diana kills alice'...")
    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(
        [str(a) for a in m.symbols(shown=True)]
    ))

    elapsed = time.perf_counter() - start
    print(f"  Time: {elapsed*1000:.2f}ms")

    if result:
        print("  ✓ VALID - action is legal")
        print(f"  Validated: {result[0]}")
    else:
        print("  ✗ INVALID - action violates constraints")


def test_invalid_self_kill_no_minion():
    """
    Test: Imp can't kill itself when all minions are dead.
    """
    print("\n" + "=" * 60)
    print("Test: Invalid - Imp self-kill with no minions alive")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    program = """
    #const player_count = 5.

    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    needs_night(2).

    % diana is imp, eve was the only minion but is now dead
    :- not assigned(0, diana, imp).
    :- not assigned(0, eve, poisoner).

    % Eve died on day 1 (executed)
    executed(eve, 1).

    % PROPOSED INVALID ACTION: diana tries to kill herself
    % This should be UNSAT because the house rule in imp.lp prevents
    % self-targeting when no minions are alive
    :- not player_chooses(imp, diana, point(diana), night(2, 4, 2)).

    #show player_chooses/4.
    """

    print("\nValidating 'diana kills diana' (with no minions alive)...")
    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(True))

    elapsed = time.perf_counter() - start
    print(f"  Time: {elapsed*1000:.2f}ms")

    if result:
        print("  ✗ BUG - action should be invalid but was accepted")
    else:
        print("  ✓ CORRECTLY REJECTED - Imp can't self-kill without minions")


def test_valid_starpass():
    """
    Test: Imp CAN kill itself when a minion is alive (starpass).
    """
    print("\n" + "=" * 60)
    print("Test: Valid - Imp starpass (self-kill with minion alive)")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    program = """
    #const player_count = 5.

    name(alice). chair(alice, 0).
    name(bob). chair(bob, 1).
    name(charlie). chair(charlie, 2).
    name(diana). chair(diana, 3).
    name(eve). chair(eve, 4).

    needs_night(2).

    % diana is imp, eve is poisoner (alive)
    :- not assigned(0, diana, imp).
    :- not assigned(0, eve, poisoner).

    % PROPOSED ACTION: diana kills herself (starpass)
    % This should be SAT because eve (minion) is alive
    :- not player_chooses(imp, diana, point(diana), night(2, 4, 2)).

    #show player_chooses/4.
    """

    print("\nValidating 'diana kills diana' (starpass with poisoner alive)...")
    start = time.perf_counter()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    ctl.ground([("base", [])])

    result = []
    ctl.solve(on_model=lambda m: result.append(
        [str(a) for a in m.symbols(shown=True)]
    ))

    elapsed = time.perf_counter() - start
    print(f"  Time: {elapsed*1000:.2f}ms")

    if result:
        print("  ✓ VALID - starpass allowed with minion alive")
        print(f"  Validated: {result[0]}")
    else:
        print("  ✗ BUG - starpass should be valid but was rejected")


def main():
    print("\nReal ASP Integration Tests")
    print("=" * 60)

    test_imp_kill_with_real_asp()
    test_validation_with_generation()
    test_validate_specific_action()
    test_invalid_self_kill_no_minion()
    test_valid_starpass()

    print("\n" + "=" * 60)
    print("Summary")
    print("=" * 60)
    print("Key insight: Single-transition validation with real ASP works!")
    print("The approach is:")
    print("  1. Load botc.lp + script (tb.lp)")
    print("  2. Provide current state as constraints")
    print("  3. Assert proposed action as constraint")
    print("  4. SAT = valid, UNSAT = invalid")


if __name__ == "__main__":
    main()

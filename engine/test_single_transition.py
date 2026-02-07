#!/usr/bin/env python3
"""
Test: Minimal single-transition validation through ASP.

Goal: Validate ONE action (e.g., "imp targets alice") using ASP,
measuring the cost and seeing what's actually needed.
"""

import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import clingo


def test_minimal_validation():
    """
    Test the absolute minimum: can we validate a single night action?

    Scenario:
    - 5 players: alice, bob, charlie, diana (imp), eve
    - Night 1
    - Imp wants to kill alice
    - Is this legal?
    """
    print("=" * 60)
    print("Test: Minimal single-transition ASP validation")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent

    # Read just the files we need for this validation
    # Start with the absolute minimum

    minimal_program = """
    % === Minimal game state ===
    player(alice; bob; charlie; diana; eve).

    % Role assignments (at setup, time index 0)
    role(imp).
    role(washerwoman).
    role(empath).
    role(monk).
    role(saint).

    assigned(diana, imp).
    assigned(alice, washerwoman).
    assigned(bob, empath).
    assigned(charlie, monk).
    assigned(eve, saint).

    % Everyone is alive
    alive(P) :- player(P).

    % Current phase
    current_phase(night(1)).

    % === Proposed action ===
    % Imp diana wants to kill alice
    proposed_kill(diana, alice).

    % === Validation rules ===
    % An imp kill is valid if:
    % 1. The killer has the imp role
    % 2. The killer is alive
    % 3. The target is a player
    % 4. The target is alive
    % 5. The target is not the killer
    % 6. It's night time

    valid_imp_kill(Killer, Target) :-
        proposed_kill(Killer, Target),
        assigned(Killer, imp),
        alive(Killer),
        player(Target),
        alive(Target),
        Killer != Target,
        current_phase(night(_)).

    invalid_reason(not_imp) :- proposed_kill(K, _), not assigned(K, imp).
    invalid_reason(killer_dead) :- proposed_kill(K, _), not alive(K).
    invalid_reason(target_not_player) :- proposed_kill(_, T), not player(T).
    invalid_reason(target_dead) :- proposed_kill(_, T), not alive(T).
    invalid_reason(self_target) :- proposed_kill(K, K).
    invalid_reason(not_night) :- proposed_kill(_, _), not current_phase(night(_)).

    transition_valid :- valid_imp_kill(_, _).
    transition_invalid :- invalid_reason(_).

    #show transition_valid/0.
    #show transition_invalid/0.
    #show invalid_reason/1.
    #show valid_imp_kill/2.
    """

    print("\nRunning minimal validation...")
    start = time.perf_counter()

    ctl = clingo.Control()
    ctl.add("base", [], minimal_program)
    ctl.ground([("base", [])])

    models = []
    ctl.solve(on_model=lambda m: models.append(
        set(str(a) for a in m.symbols(shown=True))
    ))

    elapsed = time.perf_counter() - start

    print(f"Time: {elapsed*1000:.2f}ms")
    print(f"Models found: {len(models)}")

    if models:
        model = models[0]
        print(f"Result: {model}")

        if "transition_valid" in model:
            print("\n✓ Transition is VALID")
        elif "transition_invalid" in model:
            reasons = [a for a in model if a.startswith("invalid_reason")]
            print(f"\n✗ Transition is INVALID: {reasons}")

    return elapsed


def test_invalid_scenarios():
    """Test various invalid scenarios to make sure validation catches them."""
    print("\n" + "=" * 60)
    print("Test: Invalid scenario detection")
    print("=" * 60)

    scenarios = [
        ("Self-target (imp kills self)", """
            player(diana).
            assigned(diana, imp).
            alive(diana).
            current_phase(night(1)).
            proposed_kill(diana, diana).
        """),
        ("Non-imp tries to kill", """
            player(alice; diana).
            assigned(diana, imp).
            assigned(alice, washerwoman).
            alive(alice). alive(diana).
            current_phase(night(1)).
            proposed_kill(alice, diana).
        """),
        ("Kill during day", """
            player(alice; diana).
            assigned(diana, imp).
            alive(alice). alive(diana).
            current_phase(day(1)).
            proposed_kill(diana, alice).
        """),
        ("Target already dead", """
            player(alice; diana).
            assigned(diana, imp).
            alive(diana).
            % alice is NOT alive
            current_phase(night(1)).
            proposed_kill(diana, alice).
        """),
    ]

    validation_rules = """
        valid_imp_kill(Killer, Target) :-
            proposed_kill(Killer, Target),
            assigned(Killer, imp),
            alive(Killer),
            player(Target),
            alive(Target),
            Killer != Target,
            current_phase(night(_)).

        invalid_reason(not_imp) :- proposed_kill(K, _), not assigned(K, imp).
        invalid_reason(killer_dead) :- proposed_kill(K, _), not alive(K).
        invalid_reason(target_not_player) :- proposed_kill(_, T), not player(T).
        invalid_reason(target_dead) :- proposed_kill(_, T), not alive(T).
        invalid_reason(self_target) :- proposed_kill(K, K).
        invalid_reason(not_night) :- proposed_kill(_, _), not current_phase(night(_)).

        transition_valid :- valid_imp_kill(_, _).
        transition_invalid :- invalid_reason(_).

        #show transition_valid/0.
        #show transition_invalid/0.
        #show invalid_reason/1.
    """

    for name, scenario in scenarios:
        program = scenario + validation_rules

        ctl = clingo.Control()
        ctl.add("base", [], program)
        ctl.ground([("base", [])])

        models = []
        ctl.solve(on_model=lambda m: models.append(
            set(str(a) for a in m.symbols(shown=True))
        ))

        if models:
            model = models[0]
            is_valid = "transition_valid" in model
            reasons = [a for a in model if a.startswith("invalid_reason")]

            status = "✗ VALID (BUG!)" if is_valid else f"✓ Invalid: {reasons}"
            print(f"  {name}: {status}")


def test_with_real_role_file():
    """
    Test: Can we use the actual imp.lp role definition?

    This tests whether we can integrate with existing ASP files.
    """
    print("\n" + "=" * 60)
    print("Test: Using actual role definition from imp.lp")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent
    imp_file = asp_path / "roles" / "tb" / "demons" / "imp.lp"

    if not imp_file.exists():
        print(f"  Skipping: {imp_file} not found")
        return

    # Read the actual imp definition
    imp_code = imp_file.read_text()
    print(f"  imp.lp is {len(imp_code)} bytes")

    # This is where it gets tricky - the existing imp.lp expects
    # the full botc.lp infrastructure. Let's see what predicates it uses.

    # Look for predicates used in imp.lp
    import re
    predicates_used = set(re.findall(r'\b([a-z_]+)\s*\(', imp_code))
    print(f"  Predicates used: {sorted(predicates_used)[:20]}...")  # First 20


def main():
    """Run all tests."""
    print("\nSingle-Transition Validation Tests")
    print("=" * 60)

    t1 = test_minimal_validation()
    test_invalid_scenarios()
    test_with_real_role_file()

    print("\n" + "=" * 60)
    print("Summary")
    print("=" * 60)
    print(f"Minimal validation time: {t1*1000:.2f}ms")
    print("\nNext steps:")
    print("  1. Integrate with actual role definitions")
    print("  2. Build state representation that works with existing ASP")
    print("  3. Measure cost with full role file loaded")


if __name__ == "__main__":
    main()

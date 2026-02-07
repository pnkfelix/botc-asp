#!/usr/bin/env python3
"""Debug why NRB transitions fail validation."""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import clingo


def debug_night2_imp_kill():
    """Debug why 'luke kills sullivan on night 2' fails."""
    asp_path = Path(__file__).parent.parent

    # The exact state from NRB episode 001
    program = """
    #const player_count = 9.

    % Players and seating
    name(luke). chair(luke, 0).
    name(oli). chair(oli, 1).
    name(blair). chair(blair, 2).
    name(tom). chair(tom, 3).
    name(elliott). chair(elliott, 4).
    name(laurie). chair(laurie, 5).
    name(isaac). chair(isaac, 6).
    name(jon). chair(jon, 7).
    name(sullivan). chair(sullivan, 8).

    needs_night(2).

    % Constrain role assignments to match NRB
    :- not assigned(0, luke, imp).
    :- not assigned(0, oli, ravenkeeper).
    :- not assigned(0, blair, fortune_teller).
    :- not assigned(0, tom, monk).
    :- not assigned(0, elliott, recluse).
    :- not assigned(0, laurie, scarlet_woman).
    :- not assigned(0, isaac, undertaker).
    :- not assigned(0, jon, chef).
    :- not assigned(0, sullivan, drunk).

    % Proposed action: luke (imp) kills sullivan on night 2
    % other_night_role_order(imp, 4) from base.lp
    :- not player_chooses(imp, luke, point(sullivan), night(2, 4, 2)).

    #show player_chooses/4.
    """

    print("Debugging: Why does 'luke kills sullivan on night 2' fail?")
    print()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "botc.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    print("Grounding...")
    ctl.ground([("base", [])])

    print("Solving...")
    result = []
    ctl.solve(on_model=lambda m: result.append([str(a) for a in m.symbols(shown=True)]))

    if result:
        print("SAT! Action is valid.")
        print(f"Result: {result[0]}")
    else:
        print("UNSAT - action is invalid")
        print()
        print("Let's check if the base setup is valid without the action constraint...")

    # Try without the action constraint
    program2 = """
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

    needs_night(2).

    % Just constrain assignments
    :- not assigned(0, luke, imp).
    :- not assigned(0, oli, ravenkeeper).
    :- not assigned(0, blair, fortune_teller).
    :- not assigned(0, tom, monk).
    :- not assigned(0, elliott, recluse).
    :- not assigned(0, laurie, scarlet_woman).
    :- not assigned(0, isaac, undertaker).
    :- not assigned(0, jon, chef).
    :- not assigned(0, sullivan, drunk).

    % Show what imp choices are generated
    #show player_chooses(imp, luke, point(X), night(2, 4, 2)) : player_chooses(imp, luke, point(X), night(2, 4, 2)).
    """

    print()
    print("=" * 60)
    print("Testing base setup without action constraint:")
    print()

    ctl2 = clingo.Control(["--models=1"])
    ctl2.add("base", [], program2)
    ctl2.load(str(asp_path / "botc.lp"))
    ctl2.load(str(asp_path / "tb.lp"))

    ctl2.ground([("base", [])])

    result2 = []
    ctl2.solve(on_model=lambda m: result2.append([str(a) for a in m.symbols(shown=True)]))

    if result2:
        print("SAT! Base setup is valid.")
        choices = [c for c in result2[0] if "player_chooses" in c]
        print(f"Imp choices available: {choices}")
    else:
        print("UNSAT - base setup itself is invalid")
        print()
        print("The issue is the role distribution doesn't satisfy botc.lp constraints")
        print("Let's check what valid 9-player setups look like...")

    # What does a valid 9-player setup look like?
    program3 = """
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

    #show assigned/3.
    """

    print()
    print("=" * 60)
    print("What is a valid 9-player TB setup?")
    print()

    ctl3 = clingo.Control(["--models=1"])
    ctl3.add("base", [], program3)
    ctl3.load(str(asp_path / "botc.lp"))
    ctl3.load(str(asp_path / "tb.lp"))

    ctl3.ground([("base", [])])

    result3 = []
    ctl3.solve(on_model=lambda m: result3.append([str(a) for a in m.symbols(shown=True)]))

    if result3:
        assignments = sorted([a for a in result3[0] if a.startswith("assigned(0")])
        print("Valid setup:")
        for a in assignments:
            print(f"  {a}")

        # Count role types
        townsfolk = ["washerwoman", "librarian", "investigator", "chef", "empath",
                     "fortune_teller", "undertaker", "monk", "ravenkeeper", "virgin",
                     "slayer", "soldier", "mayor"]
        outsiders = ["butler", "drunk", "recluse", "saint"]
        minions = ["poisoner", "spy", "scarlet_woman", "baron"]
        demons = ["imp"]

        counts = {"townsfolk": 0, "outsider": 0, "minion": 0, "demon": 0}
        for a in assignments:
            role = a.split(",")[2].rstrip(")")
            if role in townsfolk:
                counts["townsfolk"] += 1
            elif role in outsiders:
                counts["outsider"] += 1
            elif role in minions:
                counts["minion"] += 1
            elif role in demons:
                counts["demon"] += 1

        print()
        print(f"Role counts: {counts}")
        print("Expected for 9 players: 5 townsfolk, 2 outsiders, 1 minion, 1 demon")


if __name__ == "__main__":
    debug_night2_imp_kill()

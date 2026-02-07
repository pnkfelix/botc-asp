#!/usr/bin/env python3
"""Debug why incremental mode fails validation."""

import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent))

import clingo


def debug_incremental():
    asp_path = Path(__file__).parent.parent

    program = """
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

current_night(2).

% Current state
inc_alive(luke).
inc_alive(oli).
inc_alive(blair).
inc_alive(tom).
inc_alive(elliott).
inc_alive(laurie).
inc_alive(isaac).
inc_alive(jon).
inc_alive(sullivan).

inc_role(luke, imp).
inc_role(oli, ravenkeeper).
inc_role(blair, fortune_teller).
inc_role(tom, monk).
inc_role(elliott, recluse).
inc_role(laurie, scarlet_woman).
inc_role(isaac, undertaker).
inc_role(jon, chef).
inc_role(sullivan, drunk).

inc_minion_alive.

% Show what's being generated
#show time/1.
#show night_number/1.
#show alive/2.
#show character_assignment_state_at_time/3.
#show player_chooses/4.
#show game_active/1.
#show functioning/2.
"""

    print("=" * 60)
    print("Debugging incremental mode")
    print("=" * 60)
    print()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "incremental.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    print("Grounding...")
    ctl.ground([("base", [])])

    print("Solving...")
    atoms = []
    def on_model(m):
        for a in m.symbols(shown=True):
            atoms.append(str(a))

    result = ctl.solve(on_model=on_model)
    print(f"Result: {result}")
    print()

    # Group atoms by predicate
    groups = {}
    for a in sorted(atoms):
        pred = a.split("(")[0]
        if pred not in groups:
            groups[pred] = []
        groups[pred].append(a)

    for pred in sorted(groups.keys()):
        print(f"{pred}:")
        for a in groups[pred][:10]:  # First 10
            print(f"  {a}")
        if len(groups[pred]) > 10:
            print(f"  ... ({len(groups[pred])} total)")
        print()


def debug_with_proposed_action():
    asp_path = Path(__file__).parent.parent

    program = """
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

current_night(2).

inc_alive(luke).
inc_alive(oli).
inc_alive(blair).
inc_alive(tom).
inc_alive(elliott).
inc_alive(laurie).
inc_alive(isaac).
inc_alive(jon).
inc_alive(sullivan).

inc_role(luke, imp).
inc_role(oli, ravenkeeper).
inc_role(blair, fortune_teller).
inc_role(tom, monk).
inc_role(elliott, recluse).
inc_role(laurie, scarlet_woman).
inc_role(isaac, undertaker).
inc_role(jon, chef).
inc_role(sullivan, drunk).

inc_minion_alive.

% PROPOSED ACTION: luke kills sullivan
:- not player_chooses(imp, luke, point(sullivan), night(2, 4, 2)).

#show player_chooses/4.
"""

    print("=" * 60)
    print("Testing with proposed action constraint")
    print("=" * 60)
    print()

    ctl = clingo.Control(["--models=1"])
    ctl.add("base", [], program)
    ctl.load(str(asp_path / "incremental.lp"))
    ctl.load(str(asp_path / "tb.lp"))

    ctl.ground([("base", [])])

    atoms = []
    result = ctl.solve(on_model=lambda m: atoms.extend(str(a) for a in m.symbols(shown=True)))

    print(f"Result: {result}")
    if atoms:
        print(f"Atoms: {atoms}")
    else:
        print("UNSAT - let's check what's missing...")
        print()

        # Try without the constraint to see what choices are generated
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

current_night(2).

inc_alive(luke).
inc_alive(oli).
inc_alive(blair).
inc_alive(tom).
inc_alive(elliott).
inc_alive(laurie).
inc_alive(isaac).
inc_alive(jon).
inc_alive(sullivan).

inc_role(luke, imp).
inc_role(oli, ravenkeeper).
inc_role(blair, fortune_teller).
inc_role(tom, monk).
inc_role(elliott, recluse).
inc_role(laurie, scarlet_woman).
inc_role(isaac, undertaker).
inc_role(jon, chef).
inc_role(sullivan, drunk).

inc_minion_alive.

#show player_chooses/4.
"""
        print("Checking what player_chooses are generated without constraint...")

        ctl2 = clingo.Control(["--models=1"])
        ctl2.add("base", [], program2)
        ctl2.load(str(asp_path / "incremental.lp"))
        ctl2.load(str(asp_path / "tb.lp"))

        ctl2.ground([("base", [])])

        atoms2 = []
        result2 = ctl2.solve(on_model=lambda m: atoms2.extend(str(a) for a in m.symbols(shown=True)))

        print(f"Result: {result2}")
        imp_choices = [a for a in atoms2 if "imp" in a]
        print(f"Imp choices: {imp_choices[:5]}")
        print()

        if not imp_choices:
            print("No Imp choices generated! Checking if conditions are met...")

            # Check what conditions the Imp choice rule needs
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

current_night(2).

inc_alive(luke).
inc_alive(oli).
inc_alive(blair).
inc_alive(tom).
inc_alive(elliott).
inc_alive(laurie).
inc_alive(isaac).
inc_alive(jon).
inc_alive(sullivan).

inc_role(luke, imp).
inc_role(oli, ravenkeeper).
inc_role(blair, fortune_teller).
inc_role(tom, monk).
inc_role(elliott, recluse).
inc_role(laurie, scarlet_woman).
inc_role(isaac, undertaker).
inc_role(jon, chef).
inc_role(sullivan, drunk).

inc_minion_alive.

% Check conditions from imp.lp choice rule:
% T = night(N, RoleOrd, 2),
% night_number(N), N > 1,
% other_night_role_order(imp, RoleOrd),
% character_assignment_state_at_time(T, Imp, imp),
% alive(Imp, T),
% game_active(night(N, 0, 0)).

check_night_number(N) :- night_number(N), N > 1.
check_role_order(R) :- other_night_role_order(imp, R).
check_time_exists(T) :- T = night(N, R, 2), night_number(N), N > 1, other_night_role_order(imp, R), time(T).
check_char_state(P) :- character_assignment_state_at_time(night(2, 4, 2), P, imp).
check_alive(P) :- alive(P, night(2, 4, 2)).
check_game_active :- game_active(night(2, 0, 0)).

#show check_night_number/1.
#show check_role_order/1.
#show check_time_exists/1.
#show check_char_state/1.
#show check_alive/1.
#show check_game_active/0.
"""
            ctl3 = clingo.Control(["--models=1"])
            ctl3.add("base", [], program3)
            ctl3.load(str(asp_path / "incremental.lp"))
            ctl3.load(str(asp_path / "tb.lp"))

            ctl3.ground([("base", [])])

            checks = []
            ctl3.solve(on_model=lambda m: checks.extend(str(a) for a in m.symbols(shown=True)))

            print("Condition checks:")
            for c in sorted(checks):
                print(f"  {c}")


if __name__ == "__main__":
    debug_incremental()
    print()
    debug_with_proposed_action()

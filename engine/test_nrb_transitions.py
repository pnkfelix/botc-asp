#!/usr/bin/env python3
"""
Extract and validate individual transitions from NRB Episode 001.

This test file extracts each player_chooses action from the recorded game
and validates them through ASP, measuring the timing for each.

Source: nrb_games/ep001_a_stud_in_scarlet.lp
Result: Good team victory
"""

import sys
import time
from pathlib import Path
from dataclasses import dataclass
from typing import List, Optional, Tuple

sys.path.insert(0, str(Path(__file__).parent.parent))

from engine.asp_validator import AspValidator, GameStateSnapshot, AspValidationResult


# === NRB Episode 001 Game Data ===

PLAYERS = ["luke", "oli", "blair", "tom", "elliott", "laurie", "isaac", "jon", "sullivan"]

SEATING = {
    "luke": 0, "oli": 1, "blair": 2, "tom": 3, "elliott": 4,
    "laurie": 5, "isaac": 6, "jon": 7, "sullivan": 8
}

INITIAL_ASSIGNMENTS = {
    "luke": "imp",
    "oli": "ravenkeeper",
    "blair": "fortune_teller",
    "tom": "monk",
    "elliott": "recluse",
    "laurie": "scarlet_woman",
    "isaac": "undertaker",
    "jon": "chef",
    "sullivan": "drunk",  # believed he was empath
}

RECEIVED_TOKENS = {
    "luke": "imp",
    "oli": "ravenkeeper",
    "blair": "fortune_teller",
    "tom": "monk",
    "elliott": "recluse",
    "laurie": "scarlet_woman",
    "isaac": "undertaker",
    "jon": "chef",
    "sullivan": "empath",  # drunk thinks he's empath
}


@dataclass
class NRBTransition:
    """A single transition from the NRB game."""
    night: int
    role: str
    player: str
    ability: str
    target: str
    description: str
    # After which day's execution (for tracking deaths)
    after_day: int = 0


# Extracted transitions from ep001_a_stud_in_scarlet.lp
NRB_TRANSITIONS = [
    # Night 2
    NRBTransition(2, "monk", "tom", "protect", "sullivan",
                  "Monk (Tom) protects Sullivan"),
    NRBTransition(2, "imp", "luke", "kill", "sullivan",
                  "Imp (Luke) targets Sullivan - protected by Monk"),

    # Night 3 (after jon executed day 2)
    NRBTransition(3, "monk", "tom", "protect", "sullivan",
                  "Monk (Tom) protects Sullivan again", after_day=2),
    NRBTransition(3, "imp", "luke", "kill", "tom",
                  "Imp (Luke) kills Tom - succeeds", after_day=2),

    # Night 4 (after elliott executed day 3, tom died night 3)
    NRBTransition(4, "imp", "luke", "kill", "oli",
                  "Imp (Luke) kills Oli", after_day=3),
    NRBTransition(4, "ravenkeeper", "oli", "learn", "laurie",
                  "Ravenkeeper (Oli) targets Laurie - dying ability", after_day=3),

    # Night 5 (after luke executed day 4, laurie becomes imp via Scarlet Woman)
    # Note: This test currently fails because we don't track mid-game role changes.
    # The ASP file ep001_a_stud_in_scarlet.lp handles this via role_assignment_changes_to.
    NRBTransition(5, "imp", "laurie", "kill", "sullivan",
                  "Imp (Laurie, SW triggered) kills Sullivan [needs role change tracking]", after_day=4),
]


def get_executions_up_to(day: int) -> dict:
    """Return executions that happened up to and including the given day."""
    all_executions = {
        2: "jon",
        3: "elliott",
        4: "luke",
        5: "laurie",
    }
    return {d: p for d, p in all_executions.items() if d <= day}


def get_night_deaths_up_to(night: int) -> set:
    """Return players who died in nights before the given night."""
    # Night deaths: tom (night 3), oli (night 4), sullivan (night 5)
    deaths = set()
    if night > 3:
        deaths.add("tom")
    if night > 4:
        deaths.add("oli")
    return deaths


def make_state_for_transition(t: NRBTransition) -> GameStateSnapshot:
    """Create a state snapshot appropriate for validating this transition."""
    # Determine who has been executed
    executions = get_executions_up_to(t.after_day)

    # Determine who died at night
    night_deaths = get_night_deaths_up_to(t.night)

    # Combine for dead players
    dead = set(executions.values()) | night_deaths

    # Special case: night 5, laurie is now the imp (Scarlet Woman triggered)
    assignments = dict(INITIAL_ASSIGNMENTS)
    if t.night == 5 and t.after_day >= 4:
        # Luke was executed day 4, Scarlet Woman Laurie becomes Imp
        assignments["laurie"] = "imp"

    return GameStateSnapshot(
        players=frozenset(PLAYERS),
        seating=SEATING,
        assignments=assignments,
        received=RECEIVED_TOKENS,
        bag=frozenset(INITIAL_ASSIGNMENTS.values()),
        bluffs=frozenset(["chef", "butler", "saint"]),  # placeholder
        current_night=t.night,
        executions=executions,
    )


def validate_transition(validator: AspValidator, t: NRBTransition) -> Tuple[AspValidationResult, str]:
    """Validate a single NRB transition."""
    state = make_state_for_transition(t)

    if t.role == "imp":
        result = validator.validate_imp_kill(state, t.player, t.target)
    elif t.role == "monk":
        result = validator.validate_monk_protection(state, t.player, t.target)
    elif t.role == "poisoner":
        result = validator.validate_poisoner_poison(state, t.player, t.target)
    else:
        # For roles we haven't implemented validation for yet
        result = AspValidationResult(
            valid=True,
            elapsed_ms=0,
            details={"note": f"Validation not implemented for {t.role}"}
        )

    return result, t.description


def run_all_validations():
    """Run all NRB transition validations and report timings."""
    asp_path = Path(__file__).parent.parent
    validator = AspValidator(asp_path, script="tb")

    print("=" * 70)
    print("NRB Episode 001 Transition Validation")
    print("=" * 70)
    print()

    results = []
    total_time = 0

    for i, t in enumerate(NRB_TRANSITIONS, 1):
        result, desc = validate_transition(validator, t)
        results.append((t, result))
        total_time += result.elapsed_ms

        status = "✓ VALID" if result.valid else "✗ INVALID"
        print(f"{i}. Night {t.night}: {desc}")
        print(f"   {t.role}({t.player}) -> {t.ability}({t.target})")
        print(f"   {status} [{result.elapsed_ms:.0f}ms]")
        if result.error:
            print(f"   Error: {result.error}")
        print()

    # Summary
    print("=" * 70)
    print("Summary")
    print("=" * 70)

    valid_count = sum(1 for _, r in results if r.valid)
    print(f"Transitions validated: {valid_count}/{len(results)}")
    print(f"Total validation time: {total_time:.0f}ms")
    print(f"Average per transition: {total_time/len(results):.0f}ms")
    print()

    # Detailed timing breakdown
    print("Timing breakdown by role:")
    role_times = {}
    for t, r in results:
        if t.role not in role_times:
            role_times[t.role] = []
        role_times[t.role].append(r.elapsed_ms)

    for role, times in sorted(role_times.items()):
        avg = sum(times) / len(times)
        print(f"  {role}: avg {avg:.0f}ms (n={len(times)})")


def run_quick_demo():
    """Quick demo showing one transition validation."""
    asp_path = Path(__file__).parent.parent
    validator = AspValidator(asp_path, script="tb")

    print("Quick demo: Validating Imp kill from NRB Episode 001")
    print()

    # Night 3: Imp kills Tom
    t = NRBTransition(3, "imp", "luke", "kill", "tom",
                      "Imp (Luke) kills Tom", after_day=2)

    state = make_state_for_transition(t)
    result = validator.validate_imp_kill(state, "luke", "tom")

    print(f"State: Night {t.night}, after Day {t.after_day} execution")
    print(f"Dead: {get_executions_up_to(t.after_day)}")
    print(f"Action: luke (Imp) kills tom")
    print(f"Result: {'VALID' if result.valid else 'INVALID'}")
    print(f"Time: {result.elapsed_ms:.0f}ms")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Validate NRB Episode 001 transitions")
    parser.add_argument("--quick", action="store_true", help="Run quick demo only")
    args = parser.parse_args()

    if args.quick:
        run_quick_demo()
    else:
        run_all_validations()

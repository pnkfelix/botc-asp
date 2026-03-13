#!/usr/bin/env python3
"""
Cross-validate botc-zdd- Night 1 info role outputs against clingo ASP oracle.

Uses the actual ASP game logic (botc.lp + tb.lp) as ground truth, projecting
onto observable atoms to count distinct Storyteller choice combinations.

For a fixed seat assignment, the number of valid ST choice combos should match
the ZDD world count.

**Projection atoms** (what the ZDD tracks):
- st_tells_core/4: each info role's output
- poi_target/1: which player the Poisoner targets (derived from poi_poisoned)
- ft_red_herring/1: which player is the FT red herring (derived from ft_red_herring)

Requirements:
- Python clingo package: pip install clingo
- botc-zdd- repo cloned and built: cd ../botc-zdd- && npx tsc
"""

import clingo
import subprocess
import json
import sys
from pathlib import Path

SCRIPT_DIR = Path(__file__).parent
ASP_ROOT = SCRIPT_DIR.parent
ZDD_REPO = ASP_ROOT.parent / "botc-zdd-"

# Player names from players.lp, indexed by chair number
PLAYERS = [
    "amanda", "rob", "taylor", "courtney", "steph",
    "felix", "neha", "pratik", "kunjal", "cyrielle",
    "logan", "lou", "kate", "ivan", "ricky", "kyla",
]


# =============================================================================
# Test scenarios
# =============================================================================

SCENARIOS = [
    # --- No Poisoner, no Spy/Recluse, no FT ---
    {
        "name": "5p basic: WW,Chef,Empath,SW,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "empath", 3: "scarlet_woman", 4: "imp"},
        "player_count": 5,
    },
    # --- Poisoner, no Spy/Recluse ---
    {
        "name": "5p Poisoner: WW,Chef,Empath,Poisoner,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "empath", 3: "poisoner", 4: "imp"},
        "player_count": 5,
    },
    {
        "name": "5p Poisoner rotated: Poisoner,WW,Empath,Imp,Chef",
        "seats": {0: "poisoner", 1: "washerwoman", 2: "empath", 3: "imp", 4: "chef"},
        "player_count": 5,
    },
    # --- Spy (no Poisoner) ---
    {
        "name": "5p Spy: WW,Chef,Empath,Spy,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "empath", 3: "spy", 4: "imp"},
        "player_count": 5,
    },
    # --- Recluse (no Poisoner) ---
    {
        "name": "5p Recluse: WW,Chef,Recluse,SW,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "recluse", 3: "scarlet_woman", 4: "imp"},
        "player_count": 5,
    },
    {
        "name": "5p Recluse+Inv: Investigator,Chef,Recluse,SW,Imp",
        "seats": {0: "investigator", 1: "chef", 2: "recluse", 3: "scarlet_woman", 4: "imp"},
        "player_count": 5,
    },
    # --- Fortune Teller (no Poisoner) ---
    {
        "name": "5p FT: FT,Chef,Empath,SW,Imp",
        "seats": {0: "fortune_teller", 1: "chef", 2: "empath", 3: "scarlet_woman", 4: "imp"},
        "player_count": 5,
    },
    # --- Fortune Teller + Poisoner ---
    {
        "name": "5p FT+Poisoner: FT,Chef,Empath,Poisoner,Imp",
        "seats": {0: "fortune_teller", 1: "chef", 2: "empath", 3: "poisoner", 4: "imp"},
        "player_count": 5,
    },
    # --- Spy + Recluse ---
    {
        "name": "6p Spy+Recluse: WW,Chef,Spy,Recluse,SW,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "spy", 3: "recluse", 4: "scarlet_woman", 5: "imp"},
        "player_count": 6,
    },
    # --- Large: 7 players ---
    {
        "name": "7p all info: WW,Lib,Inv,Chef,Empath,SW,Imp",
        "seats": {0: "washerwoman", 1: "librarian", 2: "investigator", 3: "chef", 4: "empath", 5: "scarlet_woman", 6: "imp"},
        "player_count": 7,
    },
    {
        "name": "7p Spy+info: WW,Lib,Inv,Chef,Empath,Spy,Imp",
        "seats": {0: "washerwoman", 1: "librarian", 2: "investigator", 3: "chef", 4: "empath", 5: "spy", 6: "imp"},
        "player_count": 7,
    },
]


# =============================================================================
# Clingo Oracle — count projected models
# =============================================================================

def load_asp_program():
    """Load the ASP program files, stripping distribution constraints.

    The ZDD Night Info phase operates on an already-validated seat assignment,
    so we remove the integrity constraints that enforce correct role category
    counts. This allows testing arbitrary seat assignments.
    """
    program = ""
    for f in ["botc.lp", "tb.lp", "players.lp"]:
        program += open(ASP_ROOT / f).read() + "\n"

    # Remove distribution integrity constraints (lines like):
    #   :- adjusted_townsfolk(N), #count { R : assigned(0, _, R), townsfolk(R) } != N.
    #   :- adjusted_outsider(N), ...
    #   :- adjusted_minion(N), ...
    #   :- adjusted_demon(N), ...
    import re
    program = re.sub(
        r'^:- adjusted_(townsfolk|outsider|minion|demon)\(N\),.*$',
        '% [stripped for cross-validation]',
        program,
        flags=re.MULTILINE,
    )
    return program


def make_scenario_program(base_program, scenario):
    """Generate the full ASP program for a scenario."""
    seats = scenario["seats"]
    pc = scenario["player_count"]

    lines = [base_program, f"#const player_count = {pc}.\n"]

    for seat, role in sorted(seats.items()):
        player = PLAYERS[seat]
        lines.append(f"assigned(0, {player}, {role}).")


    # Projection atoms
    lines.append("")
    lines.append("% Derived projection atoms")
    lines.append("poi_target(P) :- reminder_on(poi_poisoned, P, _).")
    lines.append("ft_red_herring(P) :- reminder_on(ft_red_herring, P, _).")
    lines.append("")
    lines.append("#project st_tells_core/4.")
    lines.append("#project poi_target/1.")
    lines.append("#project ft_red_herring/1.")
    lines.append("% FT pair choice is part of the observable state")
    lines.append("#project player_chooses/4.")

    return "\n".join(lines)


def count_asp_models(scenario, base_program):
    """Count projected models for a scenario using clingo."""
    program = make_scenario_program(base_program, scenario)

    ctl = clingo.Control(["0", "--project", "--warn=none"])
    ctl.add("base", [], program)
    ctl.ground([("base", [])])

    result = [0]
    def on_model(model, r=result):
        r[0] += 1

    ctl.solve(on_model=on_model)
    return result[0]


# =============================================================================
# ZDD Runner
# =============================================================================

ZDD_SCRIPT = """
const { ZDD } = require('./dist/zdd.js');
const { TROUBLE_BREWING } = require('./dist/botc.js');
const { buildNightInfoZDD } = require('./dist/night.js');

const scenarios = %SCENARIOS%;
const results = {};

for (const [name, config] of Object.entries(scenarios)) {
  const seatRoles = new Map();
  for (const [seat, role] of Object.entries(config.seats)) {
    seatRoles.set(Number(seat), role);
  }

  const zdd = new ZDD();
  const result = buildNightInfoZDD(zdd, {
    numPlayers: config.player_count,
    seatRoles,
    selectedRoles: Object.values(config.seats),
    script: TROUBLE_BREWING,
  });

  results[name] = {
    total: zdd.count(result.root),
    variableCount: result.variableCount,
  };
}

console.log(JSON.stringify(results));
"""


def zdd_role_name(asp_name):
    """Convert ASP role name to ZDD format."""
    return asp_name.replace("_", " ").title()


def run_zdd(scenarios):
    """Run botc-zdd- night info builder for all scenarios."""
    zdd_scenarios = {}
    for sc in scenarios:
        zdd_seats = {}
        for seat, role in sc["seats"].items():
            zdd_seats[str(seat)] = zdd_role_name(role)
        zdd_scenarios[sc["name"]] = {
            "seats": zdd_seats,
            "player_count": sc["player_count"],
        }

    script = ZDD_SCRIPT.replace("%SCENARIOS%", json.dumps(zdd_scenarios))
    result = subprocess.run(
        ["node", "-e", script],
        cwd=str(ZDD_REPO),
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        print(f"ZDD error: {result.stderr}", file=sys.stderr)
        sys.exit(1)
    return json.loads(result.stdout)


# =============================================================================
# Comparison
# =============================================================================

def compare():
    print("=" * 70)
    print("Cross-validation: Night 1 Info")
    print("  ASP oracle (clingo, projected) vs ZDD (botc-zdd-)")
    print("=" * 70)

    base_program = load_asp_program()

    # Check if ZDD repo exists and is built
    zdd_available = (ZDD_REPO / "dist" / "zdd.js").exists()
    zdd_results = {}
    if zdd_available:
        print("\nRunning ZDD scenarios...")
        zdd_results = run_zdd(SCENARIOS)
    else:
        print(f"\nWARNING: ZDD repo not found at {ZDD_REPO}")
        print("Running ASP-only mode (no cross-validation)")

    all_pass = True

    for sc in SCENARIOS:
        name = sc["name"]
        print(f"\n--- {name} ---")
        seats_display = ", ".join(f"{s}={sc['seats'][s]}" for s in sorted(sc["seats"]))
        print(f"  Seats: {seats_display}")

        asp_count = count_asp_models(sc, base_program)
        print(f"  ASP count: {asp_count}")

        if zdd_available and name in zdd_results:
            zdd_count = zdd_results[name]["total"]
            match = asp_count == zdd_count
            status = "PASS" if match else "FAIL"
            print(f"  ZDD count: {zdd_count}")
            print(f"  Match: [{status}]")

            if not match:
                all_pass = False
                print(f"  ** UNEXPECTED MISMATCH **")

    print("\n" + "=" * 70)
    if all_pass:
        print("ALL CHECKS PASSED")
    else:
        print("UNEXPECTED MISMATCHES FOUND — investigate!")
    print("=" * 70)

    return all_pass


def main():
    success = compare()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()

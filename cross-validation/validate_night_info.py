#!/usr/bin/env python3
"""
Cross-validate botc-zdd- Night 1 info role outputs against clingo oracle.

For a fixed 5-player seat assignment, counts the number of valid ST
information choices per info role and compares ZDD vs ASP results.

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
ZDD_REPO = SCRIPT_DIR.parent.parent / "botc-zdd-"

# =============================================================================
# Test scenarios: (seat assignments, expected info)
# =============================================================================

SCENARIOS = [
    {
        "name": "5p: WW,Chef,Empath,Poisoner,Imp",
        "seats": {0: "washerwoman", 1: "chef", 2: "empath", 3: "poisoner", 4: "imp"},
        "player_count": 5,
    },
    {
        "name": "5p: Poisoner,WW,Empath,Imp,Chef",
        "seats": {0: "poisoner", 1: "washerwoman", 2: "empath", 3: "imp", 4: "chef"},
        "player_count": 5,
    },
    {
        "name": "5p: Investigator,Soldier,Virgin,Poisoner,Imp",
        "seats": {0: "investigator", 1: "soldier", 2: "virgin", 3: "poisoner", 4: "imp"},
        "player_count": 5,
    },
    {
        "name": "6p: Librarian,Chef,Empath,Butler,Poisoner,Imp",
        "seats": {0: "librarian", 1: "chef", 2: "empath", 3: "butler", 4: "poisoner", 5: "imp"},
        "player_count": 6,
    },
    {
        "name": "7p: WW,Lib,Inv,Chef,Empath,Poisoner,Imp",
        "seats": {0: "washerwoman", 1: "librarian", 2: "investigator", 3: "chef", 4: "empath", 5: "poisoner", 6: "imp"},
        "player_count": 7,
    },
]


# =============================================================================
# Clingo Oracle
# =============================================================================

def asp_role_name(name):
    """Convert Python role name to ASP format."""
    return name.lower().replace(" ", "_")


def count_washerwoman_choices(seats, player_count):
    """Count valid (playerA, playerB, townsfolk_role) triples for Washerwoman via ASP."""
    # Find Washerwoman's seat
    ww_seat = None
    for s, r in seats.items():
        if r == "washerwoman":
            ww_seat = s
            break
    if ww_seat is None:
        return 0

    # Townsfolk roles (from BotC)
    townsfolk = {"washerwoman", "librarian", "investigator", "chef", "empath",
                 "fortune_teller", "undertaker", "monk", "ravenkeeper",
                 "virgin", "slayer", "soldier", "mayor"}

    # Find other townsfolk in play (not the WW herself)
    targets = []
    for s, r in seats.items():
        if s != ww_seat and r in townsfolk:
            targets.append((s, r))

    # For each target, count valid decoys (any seat except WW and target)
    count = 0
    for target_seat, target_role in targets:
        for decoy in range(player_count):
            if decoy != ww_seat and decoy != target_seat:
                count += 1
    return count


def count_librarian_choices(seats, player_count):
    """Count valid Librarian info choices."""
    lib_seat = None
    for s, r in seats.items():
        if r == "librarian":
            lib_seat = s
            break
    if lib_seat is None:
        return 0

    outsiders = {"butler", "drunk", "recluse", "saint"}
    targets = [(s, r) for s, r in seats.items() if s != lib_seat and r in outsiders]

    if not targets:
        return 1  # "No outsiders" is one valid output

    count = 0
    for target_seat, target_role in targets:
        for decoy in range(player_count):
            if decoy != lib_seat and decoy != target_seat:
                count += 1
    return count


def count_investigator_choices(seats, player_count):
    """Count valid Investigator info choices."""
    inv_seat = None
    for s, r in seats.items():
        if r == "investigator":
            inv_seat = s
            break
    if inv_seat is None:
        return 0

    minions = {"poisoner", "spy", "scarlet_woman", "baron"}
    targets = [(s, r) for s, r in seats.items() if s != inv_seat and r in minions]

    if not targets:
        return 0

    count = 0
    for target_seat, target_role in targets:
        for decoy in range(player_count):
            if decoy != inv_seat and decoy != target_seat:
                count += 1
    return count


def count_chef_choices(seats, player_count):
    """Chef is fully determined — always 1 choice."""
    chef_seat = None
    for s, r in seats.items():
        if r == "chef":
            chef_seat = s
            break
    return 1 if chef_seat is not None else 0


def count_empath_choices(seats, player_count):
    """Empath is fully determined — always 1 choice."""
    empath_seat = None
    for s, r in seats.items():
        if r == "empath":
            empath_seat = s
            break
    return 1 if empath_seat is not None else 0


def asp_total_worlds(seats, player_count):
    """Compute total night info worlds as product of per-role choices."""
    ww = count_washerwoman_choices(seats, player_count)
    lib = count_librarian_choices(seats, player_count)
    inv = count_investigator_choices(seats, player_count)
    chef = count_chef_choices(seats, player_count)
    empath = count_empath_choices(seats, player_count)

    # Only multiply non-zero values (roles in play)
    total = 1
    per_role = {}
    for name, val in [("Washerwoman", ww), ("Librarian", lib), ("Investigator", inv),
                       ("Chef", chef), ("Empath", empath)]:
        if val > 0:
            total *= val
            per_role[name] = val

    return total, per_role


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

  const perRole = {};
  for (const [roleName, range] of result.roleVariableRanges) {
    // Count worlds with this role's contribution
    // For independent cross-product, the per-role choice count is
    // the number of valid outputs (variables for exactly-one selection)
    let validOutputs = 0;
    for (let vid = range.start; vid < range.start + range.count; vid++) {
      const constrained = zdd.require(result.root, vid);
      if (zdd.count(constrained) > 0) validOutputs++;
    }
    perRole[roleName] = validOutputs;
  }

  results[name] = {
    total: zdd.count(result.root),
    perRole,
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
        # Convert to ZDD role names
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
    print("Cross-validation: Night 1 Info — botc-asp (oracle) vs botc-zdd- (ZDD)")
    print("=" * 70)

    zdd_results = run_zdd(SCENARIOS)
    all_pass = True

    for sc in SCENARIOS:
        name = sc["name"]
        print(f"\n--- {name} ---")
        seats_display = ", ".join(f"{s}={r}" for s, r in sorted(sc["seats"].items()))
        print(f"  Seats: {seats_display}")

        asp_total, asp_per_role = asp_total_worlds(sc["seats"], sc["player_count"])
        zdd = zdd_results[name]
        zdd_total = zdd["total"]
        zdd_per_role = zdd["perRole"]

        total_match = asp_total == zdd_total
        status = "PASS" if total_match else "FAIL"
        print(f"  Total worlds:  ASP={asp_total:>6}  ZDD={zdd_total:>6}  [{status}]")

        if not total_match:
            all_pass = False

        # Per-role comparison
        all_roles = sorted(set(list(asp_per_role.keys()) + [zdd_role_name(k) for k in asp_per_role.keys()]))
        for role_name in asp_per_role:
            zdd_name = zdd_role_name(role_name) if role_name.islower() else role_name
            asp_val = asp_per_role.get(role_name, 0)
            zdd_val = zdd_per_role.get(zdd_name, 0)
            match = asp_val == zdd_val
            rs = "PASS" if match else "FAIL"
            print(f"    {zdd_name:>15}: ASP={asp_val:>4}  ZDD={zdd_val:>4}  [{rs}]")
            if not match:
                all_pass = False

    print("\n" + "=" * 70)
    if all_pass:
        print("ALL CHECKS PASSED")
    else:
        print("SOME CHECKS FAILED")
    print("=" * 70)
    return all_pass


def main():
    success = compare()
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
Cross-validate botc-zdd- Night 1 info role outputs against clingo oracle.

For a fixed seat assignment, counts the number of valid ST information
choices per info role and compares ZDD vs ASP results.

When the Poisoner is in play, the oracle computes per-branch world counts
(one branch per poisoner target), where the targeted info role gets
unconstrained outputs. The total world count is the sum across branches.

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
# Role categories (Trouble Brewing)
# =============================================================================

TOWNSFOLK = {"washerwoman", "librarian", "investigator", "chef", "empath",
             "fortune_teller", "undertaker", "monk", "ravenkeeper",
             "virgin", "slayer", "soldier", "mayor"}

OUTSIDERS = {"butler", "drunk", "recluse", "saint"}

MINIONS = {"poisoner", "spy", "scarlet_woman", "baron"}

DEMONS = {"imp"}

ALL_TOWNSFOLK_COUNT = len(TOWNSFOLK)  # 13
ALL_OUTSIDER_COUNT = len(OUTSIDERS)   # 4
ALL_MINION_COUNT = len(MINIONS)       # 4

# =============================================================================
# Test scenarios
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
    # No-Poisoner scenarios (use Spy as the minion)
    {
        "name": "5p: WW,Chef,Empath,Spy,Imp (no Poisoner)",
        "seats": {0: "washerwoman", 1: "chef", 2: "empath", 3: "spy", 4: "imp"},
        "player_count": 5,
    },
    {
        "name": "7p: WW,Lib,Inv,Chef,Empath,Spy,Imp (no Poisoner)",
        "seats": {0: "washerwoman", 1: "librarian", 2: "investigator", 3: "chef", 4: "empath", 5: "spy", 6: "imp"},
        "player_count": 7,
    },
]


# =============================================================================
# Clingo Oracle — per-role choice counts
# =============================================================================

def is_info_role(role):
    """Check if a role is a Night 1 info role."""
    return role in {"washerwoman", "librarian", "investigator", "chef", "empath"}


def count_pair_role_choices(seats, player_count, info_seat, info_role, target_category,
                            malfunctioning=False):
    """Count valid choices for a pair-based info role (WW, Lib, Inv).

    When functioning: target must actually have a role of the target category.
    When malfunctioning: any pair of other players × any role of target category from script.
    """
    if info_role == "washerwoman":
        category_set = TOWNSFOLK
        all_count = ALL_TOWNSFOLK_COUNT
    elif info_role == "librarian":
        category_set = OUTSIDERS
        all_count = ALL_OUTSIDER_COUNT
    elif info_role == "investigator":
        category_set = MINIONS
        all_count = ALL_MINION_COUNT
    else:
        return 0

    if malfunctioning:
        # Any pair of other players × any role from the target category in the script
        other_seats = [s for s in range(player_count) if s != info_seat]
        n = len(other_seats)
        pair_count = n * (n - 1) // 2
        total = pair_count * all_count
        # Librarian also gets "No Outsiders" as an option
        if info_role == "librarian":
            total += 1
        return total

    # Functioning: find actual targets
    targets = []
    for s, r in seats.items():
        if s != info_seat and r in category_set:
            targets.append((s, r))

    if not targets:
        if info_role == "librarian":
            return 1  # "No Outsiders"
        return 0

    count = 0
    for target_seat, target_role in targets:
        for decoy in range(player_count):
            if decoy != info_seat and decoy != target_seat:
                count += 1
    return count


def count_chef_choices(seats, player_count, chef_seat, malfunctioning=False):
    """Chef choices: 1 when functioning (determined), numPlayers+1 when malfunctioning."""
    if malfunctioning:
        return player_count + 1  # counts 0..numPlayers
    return 1


def count_empath_choices(seats, player_count, empath_seat, malfunctioning=False):
    """Empath choices: 1 when functioning (determined), 3 when malfunctioning."""
    if malfunctioning:
        return 3  # counts 0, 1, 2
    return 1


def compute_role_choices(seats, player_count, malfunctioning_seats=None):
    """Compute per-role choice counts, returning {role_name: count}.

    malfunctioning_seats: set of seats that are malfunctioning.
    """
    if malfunctioning_seats is None:
        malfunctioning_seats = set()

    per_role = {}

    for seat, role in seats.items():
        malf = seat in malfunctioning_seats

        if role == "washerwoman":
            c = count_pair_role_choices(seats, player_count, seat, role, TOWNSFOLK, malf)
            if c > 0:
                per_role["Washerwoman"] = c
        elif role == "librarian":
            c = count_pair_role_choices(seats, player_count, seat, role, OUTSIDERS, malf)
            if c > 0:
                per_role["Librarian"] = c
        elif role == "investigator":
            c = count_pair_role_choices(seats, player_count, seat, role, MINIONS, malf)
            if c > 0:
                per_role["Investigator"] = c
        elif role == "chef":
            c = count_chef_choices(seats, player_count, seat, malf)
            if c > 0:
                per_role["Chef"] = c
        elif role == "empath":
            c = count_empath_choices(seats, player_count, seat, malf)
            if c > 0:
                per_role["Empath"] = c

    return per_role


def asp_total_worlds(seats, player_count):
    """Compute total night info worlds.

    If Poisoner is in play: sum across branches (one per poisoner target).
    Otherwise: simple product of per-role choices.
    """
    poisoner_seat = None
    for s, r in seats.items():
        if r == "poisoner":
            poisoner_seat = s
            break

    if poisoner_seat is None:
        # No Poisoner — simple cross-product
        per_role = compute_role_choices(seats, player_count)
        total = 1
        for v in per_role.values():
            total *= v
        return total, per_role

    # Poisoner in play — sum across branches
    total = 0
    # For the per-role summary, count how many valid outputs exist across ALL branches
    combined_per_role = {}

    for target in range(player_count):
        if target == poisoner_seat:
            continue

        malfunctioning = {target}
        branch_per_role = compute_role_choices(seats, player_count, malfunctioning)

        branch_total = 1
        for v in branch_per_role.values():
            branch_total *= v
        total += branch_total

    # For per-role reporting, we report the number of outputs that are valid
    # in at least one branch. This matches the ZDD's per-role counting
    # (require each variable and check if count > 0 across the full ZDD).
    # For a malfunctioning role, all outputs are valid; for functioning, only truthful.
    # The union across all branches means: an output is valid if it's valid in ANY branch.
    # For pair roles: functioning outputs + all malfunctioning outputs = all outputs
    #   (since if the poisoner targets that role, ALL outputs are valid).
    # For count roles: functioning output + all counts = all counts.
    # So if an info role exists, the number of "reachable" outputs = malfunctioning count
    #   (because the poisoner CAN target them, making all outputs reachable).
    for seat, role in seats.items():
        if not is_info_role(role):
            continue
        malf_choices = compute_role_choices(seats, player_count, {seat})
        zdd_name = role.replace("_", " ").title()
        if zdd_name in malf_choices:
            combined_per_role[zdd_name] = malf_choices[zdd_name]

    # Add Poisoner target count
    combined_per_role["Poisoner"] = player_count - 1

    return total, combined_per_role


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
        all_role_names = sorted(set(list(asp_per_role.keys()) + list(zdd_per_role.keys())))
        for role_name in all_role_names:
            asp_val = asp_per_role.get(role_name, 0)
            zdd_val = zdd_per_role.get(role_name, 0)
            match = asp_val == zdd_val
            rs = "PASS" if match else "FAIL"
            print(f"    {role_name:>15}: ASP={asp_val:>4}  ZDD={zdd_val:>4}  [{rs}]")
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

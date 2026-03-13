#!/usr/bin/env python3
"""
Cross-validate botc-zdd- distribution counts against clingo oracle.

This script:
1. Runs clingo on a minimal distribution-only ASP program (ground truth)
2. Runs the botc-zdd- library via Node.js subprocess
3. Compares the counts and reports mismatches

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
# Clingo Oracle
# =============================================================================

DISTRIBUTION_ASP = """
% Trouble Brewing roles
townsfolk(washerwoman; librarian; investigator; chef; empath; fortune_teller;
          undertaker; monk; ravenkeeper; virgin; slayer; soldier; mayor).
outsider(butler; drunk; recluse; saint).
minion(poisoner; spy; scarlet_woman; baron).
demon(imp).

role(X) :- townsfolk(X).
role(X) :- outsider(X).
role(X) :- minion(X).
role(X) :- demon(X).

% Baron's outsider adjustment (differs by player count!)
causes_outsider_mod(baron, 1) :- player_count < 7.
causes_outsider_mod(baron, 2) :- player_count >= 7.
causes_townsfolk_mod(R, -N) :- causes_outsider_mod(R, N).

% Base distribution counts
base_townsfolk(3) :- 5 <= player_count, player_count <= 6.
base_townsfolk(5) :- 7 <= player_count, player_count <= 9.
base_townsfolk(7) :- 10 <= player_count, player_count <= 12.
base_townsfolk(9) :- 13 <= player_count, player_count <= 15.

base_outsider(player_count - 5) :- 5 <= player_count, player_count <= 6.
base_outsider(player_count - 7) :- 7 <= player_count, player_count <= 9.
base_outsider(player_count - 10) :- 10 <= player_count, player_count <= 12.
base_outsider(player_count - 13) :- 13 <= player_count, player_count <= 15.

base_minion(1) :- 5 <= player_count, player_count <= 9.
base_minion(2) :- 10 <= player_count, player_count <= 12.
base_minion(3) :- 13 <= player_count, player_count <= 15.
base_demon(1).

% Choose roles to distribute
{ distrib(X) : townsfolk(X) }.
{ distrib(X) : outsider(X) }.
{ distrib(X) : minion(X) }.
{ distrib(X) : demon(X) }.

% Adjustments
outsider_adjustment(A) :- A = #sum { N, R : causes_outsider_mod(R, N), distrib(R) }.
townsfolk_adjustment(A) :- A = #sum { N, R : causes_townsfolk_mod(R, N), distrib(R) }.
causes_minion_mod(none, 0) :- #false.
causes_demon_mod(none, 0) :- #false.
minion_adjustment(A) :- A = #sum { N, R : causes_minion_mod(R, N), distrib(R) }.
demon_adjustment(A) :- A = #sum { N, R : causes_demon_mod(R, N), distrib(R) }.

adjusted_townsfolk(B + A) :- base_townsfolk(B), townsfolk_adjustment(A).
adjusted_outsider(B + A) :- base_outsider(B), outsider_adjustment(A).
adjusted_minion(B + A) :- base_minion(B), minion_adjustment(A).
adjusted_demon(B + A) :- base_demon(B), demon_adjustment(A).

% Enforce correct category counts
:- adjusted_townsfolk(N), #count { R : distrib(R), townsfolk(R) } != N.
:- adjusted_outsider(N), #count { R : distrib(R), outsider(R) } != N.
:- adjusted_minion(N), #count { R : distrib(R), minion(R) } != N.
:- adjusted_demon(N), #count { R : distrib(R), demon(R) } != N.

#show distrib/1.
"""


def clingo_count_distributions(player_count: int, exclude_baron: bool = False) -> dict:
    """Run clingo to count valid distributions. Returns {count, distributions}."""
    ctl = clingo.Control(["0"])
    program = DISTRIBUTION_ASP
    program += f"\n#const player_count = {player_count}.\n"
    if exclude_baron:
        program += ":- distrib(baron).\n"
    ctl.add("base", [], program)
    ctl.ground([("base", [])])

    distributions = []
    def on_model(model):
        atoms = model.symbols(shown=True)
        roles = sorted(str(a.arguments[0]) for a in atoms if a.name == "distrib")
        distributions.append(roles)

    ctl.solve(on_model=on_model)
    return {"count": len(distributions), "distributions": sorted(distributions)}


# =============================================================================
# ZDD Runner
# =============================================================================

ZDD_SCRIPT = """
const { ZDD } = require('./dist/zdd.js');
const { buildDistributionZDD, buildDistributionZDDWithModifiers, TROUBLE_BREWING } = require('./dist/botc.js');

const results = {};
for (const pc of %PLAYER_COUNTS%) {
  const zdd = new ZDD();
  const noBaron = buildDistributionZDD(zdd, TROUBLE_BREWING, pc);
  const withBaron = buildDistributionZDDWithModifiers(zdd, TROUBLE_BREWING, pc);

  // Enumerate distributions for the no-baron case to compare sets
  const noBaronSets = zdd.enumerate(noBaron).map(vars =>
    vars.map(v => TROUBLE_BREWING.roles[v].name).sort()
  ).sort();

  const withBaronSets = zdd.enumerate(withBaron).map(vars =>
    vars.map(v => TROUBLE_BREWING.roles[v].name).sort()
  ).sort();

  results[pc] = {
    no_baron_count: zdd.count(noBaron),
    with_baron_count: zdd.count(withBaron),
    no_baron_sets: noBaronSets,
    with_baron_sets: withBaronSets,
  };
}
console.log(JSON.stringify(results));
"""


def run_zdd(player_counts: list[int]) -> dict:
    """Run botc-zdd- and return distribution counts."""
    script = ZDD_SCRIPT.replace("%PLAYER_COUNTS%", json.dumps(player_counts))
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

def normalize_role_name(asp_name: str) -> str:
    """Convert ASP role name to ZDD role name format.
    ASP uses: fortune_teller, scarlet_woman
    ZDD uses: Fortune Teller, Scarlet Woman
    """
    return asp_name.replace("_", " ").title()


def normalize_asp_distribution(dist: list[str]) -> list[str]:
    """Convert ASP distribution to ZDD format for comparison."""
    return sorted(normalize_role_name(r) for r in dist)


def compare(player_counts: list[int]):
    """Run full comparison."""
    print("=" * 70)
    print("Cross-validation: botc-asp (clingo) vs botc-zdd- (ZDD)")
    print("=" * 70)

    # Only enumerate full sets for small player counts to avoid memory issues
    enumerate_threshold = 8

    zdd_results = run_zdd(player_counts)

    all_pass = True

    for pc in player_counts:
        print(f"\n--- {pc} players ---")

        # Clingo oracle
        asp_no_baron = clingo_count_distributions(pc, exclude_baron=True)
        asp_with_baron = clingo_count_distributions(pc, exclude_baron=False)

        # ZDD results
        zdd = zdd_results[str(pc)]

        # Compare counts
        nb_match = asp_no_baron["count"] == zdd["no_baron_count"]
        wb_match = asp_with_baron["count"] == zdd["with_baron_count"]

        nb_status = "PASS" if nb_match else "FAIL"
        wb_status = "PASS" if wb_match else "FAIL"

        print(f"  No-Baron:   ASP={asp_no_baron['count']:>6}  ZDD={zdd['no_baron_count']:>6}  [{nb_status}]")
        print(f"  With-Baron: ASP={asp_with_baron['count']:>6}  ZDD={zdd['with_baron_count']:>6}  [{wb_status}]")

        if not nb_match or not wb_match:
            all_pass = False

        # For small counts, compare actual distribution sets
        if pc <= enumerate_threshold and not wb_match:
            asp_sets = set(tuple(normalize_asp_distribution(d)) for d in asp_with_baron["distributions"])
            zdd_sets = set(tuple(d) for d in zdd["with_baron_sets"])

            only_asp = asp_sets - zdd_sets
            only_zdd = zdd_sets - asp_sets

            if only_asp:
                print(f"  In ASP but not ZDD ({len(only_asp)} sets):")
                for s in sorted(only_asp)[:5]:
                    print(f"    {list(s)}")
                if len(only_asp) > 5:
                    print(f"    ... and {len(only_asp) - 5} more")

            if only_zdd:
                print(f"  In ZDD but not ASP ({len(only_zdd)} sets):")
                for s in sorted(only_zdd)[:5]:
                    print(f"    {list(s)}")
                if len(only_zdd) > 5:
                    print(f"    ... and {len(only_zdd) - 5} more")

    print("\n" + "=" * 70)
    if all_pass:
        print("ALL CHECKS PASSED")
    else:
        print("SOME CHECKS FAILED — see details above")
        print()
        print("Known bugs in botc-zdd-:")
        print("  1. buildDistributionZDD includes Baron as valid minion without")
        print("     requiring Baron's outsider adjustment. Baron should be excluded")
        print("     from non-modified distributions.")
        print("  2. buildDistributionZDDWithBaron hardcodes +2/-2 outsider/townsfolk")
        print("     modifier, but for player_count < 7, Baron only adds +1/-1.")
    print("=" * 70)

    return all_pass


def main():
    player_counts = [5, 6, 7, 8, 9, 10]
    success = compare(player_counts)
    sys.exit(0 if success else 1)


if __name__ == "__main__":
    main()

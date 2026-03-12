#!/usr/bin/env python3
"""
Count valid TB distributions for various player counts using clingo.
This provides oracle values to cross-validate against botc-zdd-.
"""

import clingo
import sys
import json
from pathlib import Path

def count_distributions(player_count: int, include_baron: bool = True) -> dict:
    """Count valid role distributions for a given player count.

    Returns dict with:
      - count: number of valid distributions
      - distributions: list of role sets (each a sorted list of role names)
    """
    ctl = clingo.Control(["0"])  # enumerate all models

    # Load the distribution-counting ASP program
    asp_file = Path(__file__).parent / "count_distributions.lp"
    ctl.load(str(asp_file))

    # Set player count
    ctl.add("base", [], f"#const player_count = {player_count}.")

    # If not including Baron adjustment, exclude Baron from distribution
    if not include_baron:
        ctl.add("base", [], ":- distrib(baron).")

    ctl.ground([("base", [])])

    distributions = []

    def on_model(model):
        atoms = model.symbols(shown=True)
        roles = sorted([str(a.arguments[0]) for a in atoms if a.name == "distrib"])
        distributions.append(roles)

    result = ctl.solve(on_model=on_model)

    return {
        "player_count": player_count,
        "include_baron": include_baron,
        "count": len(distributions),
        "satisfiable": result.satisfiable,
        "distributions": distributions,
    }


def main():
    player_counts = [5, 6, 7, 8, 9, 10]

    results = {}

    for pc in player_counts:
        # Without Baron adjustment (Baron can be in play but no outsider swap)
        # Actually: "without Baron" means Baron is excluded entirely
        no_baron = count_distributions(pc, include_baron=False)
        with_baron = count_distributions(pc, include_baron=True)

        results[pc] = {
            "no_baron": no_baron["count"],
            "with_baron": with_baron["count"],
            "baron_only": with_baron["count"] - no_baron["count"],
        }

        print(f"\n=== {pc} players ===")
        print(f"  Without Baron: {no_baron['count']} distributions")
        print(f"  With Baron:    {with_baron['count']} distributions")
        print(f"  Baron-only:    {with_baron['count'] - no_baron['count']} additional")

        # Show a few sample distributions
        if no_baron["count"] <= 10:
            print(f"  All distributions (no Baron):")
            for d in sorted(no_baron["distributions"]):
                print(f"    {d}")

    # Output JSON for machine consumption
    json_path = Path(__file__).parent / "oracle_counts.json"

    # Also save full distribution sets for small cases
    detail = {}
    for pc in player_counts:
        no_baron = count_distributions(pc, include_baron=False)
        with_baron = count_distributions(pc, include_baron=True)
        detail[str(pc)] = {
            "no_baron": {
                "count": no_baron["count"],
                "distributions": sorted(no_baron["distributions"]),
            },
            "with_baron": {
                "count": with_baron["count"],
                "distributions": sorted(with_baron["distributions"]),
            },
        }

    with open(json_path, "w") as f:
        json.dump(detail, f, indent=2)

    print(f"\nOracle data written to {json_path}")


if __name__ == "__main__":
    main()

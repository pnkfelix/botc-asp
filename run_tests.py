#!/usr/bin/env python3
"""Run ASP test files and check sat/unsat expectations.

Usage:
    ./run_tests.py tb              # Run all tb_tests/*.lp
    ./run_tests.py bmr             # Run all bmr_tests/*.lp
    ./run_tests.py snv             # Run all snv_tests/*.lp
    ./run_tests.py path/to/test.lp # Run a single test file
    ./run_tests.py tb bmr snv      # Run multiple test suites

Test files named sat_*.lp are expected to be SATISFIABLE.
Test files named unsat_*.lp are expected to be UNSATISFIABLE.
Other .lp files (like base.lp) are skipped.
"""

import subprocess
import sys
from pathlib import Path

# Import run_clingo's finder so we don't duplicate the logic
SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))
from run_clingo import find_clingo


def run_one_test(clingo: str, test_file: Path) -> str:
    """Run a single test file. Returns 'pass', 'fail', or 'skip'."""
    name = test_file.name

    if name.startswith("sat_"):
        expected = "SATISFIABLE"
    elif name.startswith("unsat_"):
        expected = "UNSATISFIABLE"
    else:
        return "skip"

    # For sat tests, 1 model suffices (we just need satisfiability).
    # For unsat tests, 0 means exhaustive search (needed to confirm unsatisfiability).
    num_models = "1" if expected == "SATISFIABLE" else "0"

    try:
        result = subprocess.run(
            [clingo, str(test_file), num_models],
            capture_output=True, text=True, timeout=120,
        )
        output = result.stdout + result.stderr
    except subprocess.TimeoutExpired:
        print(f"  FAIL {test_file} (timeout after 120s)")
        return "fail"

    # Look for the result line
    actual = None
    for line in output.splitlines():
        line = line.strip()
        if line == "SATISFIABLE":
            actual = "SATISFIABLE"
            break
        elif line == "UNSATISFIABLE":
            actual = "UNSATISFIABLE"
            break

    if actual == expected:
        print(f"  PASS {test_file}")
        return "pass"
    else:
        actual_str = actual or "<no result found>"
        print(f"  FAIL {test_file} (expected {expected}, got {actual_str})")
        return "fail"


def main():
    if len(sys.argv) < 2:
        print("Usage: run-tests.py <script-name|test-file> ...")
        print("  script-name: tb, bmr, snv")
        print("  test-file:   path to a single .lp test file")
        sys.exit(1)

    clingo = find_clingo()
    if not clingo:
        print(
            "Error: clingo not found. Install via nix (nix develop) or your package manager.",
            file=sys.stderr,
        )
        sys.exit(1)

    total_pass = 0
    total_fail = 0
    total_skip = 0
    failures = []

    for target in sys.argv[1:]:
        target_path = Path(target)

        if target_path.is_file():
            # Single file mode
            print(f"Running: {target}")
            result = run_one_test(clingo, target_path)
            if result == "pass":
                total_pass += 1
            elif result == "fail":
                total_fail += 1
                failures.append(str(target_path))
            else:
                total_skip += 1

        else:
            # Script name mode: look for <name>_tests/ directory
            test_dir = SCRIPT_DIR / f"{target}_tests"
            if not test_dir.is_dir():
                print(f"Error: '{target}' is not a file and '{target}_tests/' not found.",
                      file=sys.stderr)
                sys.exit(1)

            test_files = sorted(test_dir.glob("*.lp"))
            print(f"Running {target} tests from {test_dir}/")
            print()

            for test_file in test_files:
                if test_file.name == "base.lp":
                    continue
                result = run_one_test(clingo, test_file)
                if result == "pass":
                    total_pass += 1
                elif result == "fail":
                    total_fail += 1
                    failures.append(str(test_file))
                elif result == "skip":
                    total_skip += 1

            print()

    print(f"Results: {total_pass} passed, {total_fail} failed, {total_skip} skipped")

    if failures:
        print()
        print("Failures:")
        for f in failures:
            print(f"  {f}")
        sys.exit(1)


if __name__ == "__main__":
    main()

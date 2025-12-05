#!/usr/bin/env python3
"""Test runner for Blood on the Clocktower ASP tests."""

import subprocess
import sys
import time
from pathlib import Path

try:
    from rich.console import Console
    from rich.table import Table
    from rich.live import Live
    RICH_AVAILABLE = True
except ImportError:
    RICH_AVAILABLE = False

TIMEOUT = 5


def run_clingo(test_file: Path) -> tuple[str | None, float]:
    """Run clingo on a test file. Returns (result, elapsed_seconds)."""
    start = time.time()
    try:
        result = subprocess.run(
            ["clingo", str(test_file), "1"],
            capture_output=True,
            text=True,
            timeout=TIMEOUT
        )
        elapsed = time.time() - start
        output = result.stdout + result.stderr
        if "UNSATISFIABLE" in output:
            return "UNSAT", elapsed
        elif "SATISFIABLE" in output:
            return "SAT", elapsed
        else:
            return None, elapsed
    except subprocess.TimeoutExpired:
        return "TIMEOUT", TIMEOUT
    except FileNotFoundError:
        print("Error: clingo not found in PATH")
        sys.exit(1)


def get_expected(test_file: Path) -> str | None:
    """Determine expected result from filename prefix."""
    name = test_file.name
    if name.startswith("sat_"):
        return "SAT"
    elif name.startswith("unsat_"):
        return "UNSAT"
    return None


def build_table(results: list, current_test: str | None = None, current_idx: int = 0, total: int = 0) -> Table:
    """Build the results table."""
    title = "Test Results"
    if current_test:
        title = f"Running ({current_idx}/{total}): {current_test}"

    table = Table(title=title)
    table.add_column("Test", style="cyan")
    table.add_column("Expected", style="blue")
    table.add_column("Actual", style="blue")
    table.add_column("Time", style="yellow")
    table.add_column("Result")

    for name, expected, actual, elapsed, passed in results:
        if passed:
            result_str = "[green]PASS[/green]"
        else:
            result_str = "[red]FAIL[/red]"
        time_str = f"{elapsed:.2f}s" if elapsed is not None else "-"
        table.add_row(name, expected or "?", actual or "ERROR", time_str, result_str)

    return table


def run_tests_rich(test_files: list[Path]) -> int:
    """Run tests with rich output."""
    console = Console()
    results = []
    total = len(test_files)

    with Live(build_table(results, None, 0, total), console=console, refresh_per_second=4) as live:
        for idx, test_file in enumerate(test_files, 1):
            live.update(build_table(results, test_file.name, idx, total))
            expected = get_expected(test_file)
            actual, elapsed = run_clingo(test_file)
            passed = (expected == actual)
            results.append((test_file.name, expected, actual, elapsed, passed))
            live.update(build_table(results, test_file.name if idx < total else None, idx, total))

    pass_count = sum(1 for r in results if r[4])
    fail_count = len(results) - pass_count

    console.print()
    if fail_count == 0:
        console.print(f"[green bold]All {pass_count} tests passed![/green bold]")
    else:
        console.print(f"[red bold]{fail_count} failed[/red bold], [green]{pass_count} passed[/green]")

    return fail_count


def run_tests_plain(test_files: list[Path]) -> int:
    """Run tests with plain output (no rich)."""
    pass_count = 0
    fail_count = 0
    total = len(test_files)

    for idx, test_file in enumerate(test_files, 1):
        print(f"[{idx}/{total}] Testing {test_file.name}...", end=" ", flush=True)
        expected = get_expected(test_file)
        actual, elapsed = run_clingo(test_file)
        passed = (expected == actual)

        if passed:
            print(f"PASS ({elapsed:.2f}s)")
            pass_count += 1
        else:
            print(f"FAIL ({elapsed:.2f}s) - expected {expected}, got {actual}")
            fail_count += 1

    print()
    print(f"Results: {pass_count} passed, {fail_count} failed")
    return fail_count


def main():
    root_dir = Path(__file__).parent

    # Find all test files in *_tests/ subdirectories
    test_files = sorted(
        list(root_dir.glob("*_tests/sat_*.lp")) + list(root_dir.glob("*_tests/unsat_*.lp"))
    )

    if not test_files:
        print("No test files found (sat_*.lp or unsat_*.lp)")
        return 0

    if RICH_AVAILABLE:
        fail_count = run_tests_rich(test_files)
    else:
        print("(Install 'rich' for nicer output: pip install rich)")
        print()
        fail_count = run_tests_plain(test_files)

    return 1 if fail_count > 0 else 0


if __name__ == "__main__":
    sys.exit(main())

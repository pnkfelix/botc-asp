#!/usr/bin/env python3
"""Test runner for Blood on the Clocktower ASP tests."""

import fcntl
import json
import socket
import statistics
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
HOSTNAME = socket.gethostname()


def get_timing_file(test_file: Path) -> Path:
    """Get the timing file path for a test (host-specific)."""
    return test_file.with_suffix(f".timings.{HOSTNAME}.json")


def load_timings(timing_file: Path) -> list[float]:
    """Load historical timings from file (with file locking)."""
    if not timing_file.exists():
        return []
    try:
        with open(timing_file, 'r') as f:
            fcntl.flock(f.fileno(), fcntl.LOCK_SH)
            try:
                return json.load(f)
            finally:
                fcntl.flock(f.fileno(), fcntl.LOCK_UN)
    except (json.JSONDecodeError, IOError):
        return []


def save_timing(timing_file: Path, timings: list[float], new_timing: float):
    """Append new timing to file (with file locking)."""
    timings = timings + [new_timing]
    # Keep last 100 timings to avoid unbounded growth
    timings = timings[-100:]
    try:
        with open(timing_file, 'w') as f:
            fcntl.flock(f.fileno(), fcntl.LOCK_EX)
            try:
                json.dump(timings, f)
            finally:
                fcntl.flock(f.fileno(), fcntl.LOCK_UN)
    except IOError:
        pass  # Silently fail if we can't write timings


def timing_summary(timings: list[float], current: float) -> tuple[str, bool]:
    """
    Generate timing summary string and outlier flag.
    Returns (summary_str, is_outlier).
    """
    if not timings:
        return "", False

    n = len(timings)
    mean = statistics.mean(timings)

    if n < 2:
        # Not enough data for std dev
        return f"(avg: {mean:.2f}s)", False

    stdev = statistics.stdev(timings)
    min_t = min(timings)
    max_t = max(timings)

    # Consider outlier if > 2 std devs from mean (and stdev is meaningful)
    is_outlier = stdev > 0.01 and abs(current - mean) > 2 * stdev

    summary = f"[{min_t:.2f}-{max_t:.2f}] avg:{mean:.2f}s"
    if stdev > 0.01:
        summary += f" sd:{stdev:.2f}"

    return summary, is_outlier


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
    table.add_column("Result")
    table.add_column("Time", style="yellow")
    table.add_column("History", style="dim")

    for name, expected, actual, elapsed, passed, history_str, is_outlier in results:
        # Show actual result, highlighted red with !!! if it doesn't match expected
        if passed:
            result_str = f"[green]{actual}[/green]"
        else:
            result_str = f"[bold red]{actual} (!!!)[/bold red]"

        time_str = f"{elapsed:.2f}s" if elapsed is not None else "-"
        if is_outlier:
            time_str = f"[bold red]{time_str}![/bold red]"
        table.add_row(name, result_str, time_str, history_str)

    return table


def run_tests_rich(test_files: list[Path]) -> int:
    """Run tests with rich output."""
    console = Console()
    results = []
    total = len(test_files)

    with Live(build_table(results, None, 0, total), console=console, refresh_per_second=4) as live:
        for idx, test_file in enumerate(test_files, 1):
            live.update(build_table(results, test_file.name, idx, total))

            # Load historical timings
            timing_file = get_timing_file(test_file)
            timings = load_timings(timing_file)

            # Run test
            expected = get_expected(test_file)
            actual, elapsed = run_clingo(test_file)
            passed = (expected == actual)

            # Calculate timing summary and save new timing
            history_str, is_outlier = timing_summary(timings, elapsed)
            save_timing(timing_file, timings, elapsed)

            results.append((test_file.name, expected, actual, elapsed, passed, history_str, is_outlier))
            live.update(build_table(results, test_file.name if idx < total else None, idx, total))

    pass_count = sum(1 for r in results if r[4])
    fail_count = len(results) - pass_count
    outlier_count = sum(1 for r in results if r[6])

    console.print()
    if fail_count == 0:
        msg = f"[green bold]All {pass_count} tests passed![/green bold]"
        if outlier_count > 0:
            msg += f" [yellow]({outlier_count} timing outlier{'s' if outlier_count > 1 else ''})[/yellow]"
        console.print(msg)
    else:
        console.print(f"[red bold]{fail_count} failed[/red bold], [green]{pass_count} passed[/green]")

    return fail_count


def run_tests_plain(test_files: list[Path]) -> int:
    """Run tests with plain output (no rich)."""
    pass_count = 0
    fail_count = 0
    outlier_count = 0
    total = len(test_files)

    for idx, test_file in enumerate(test_files, 1):
        print(f"[{idx}/{total}] Testing {test_file.name}...", end=" ", flush=True)

        # Load historical timings
        timing_file = get_timing_file(test_file)
        timings = load_timings(timing_file)

        # Run test
        expected = get_expected(test_file)
        actual, elapsed = run_clingo(test_file)
        passed = (expected == actual)

        # Calculate timing summary and save new timing
        history_str, is_outlier = timing_summary(timings, elapsed)
        save_timing(timing_file, timings, elapsed)

        if is_outlier:
            outlier_count += 1

        time_info = f"{elapsed:.2f}s"
        if is_outlier:
            time_info += " (!)"
        if history_str:
            time_info += f" {history_str}"

        if passed:
            print(f"PASS ({time_info})")
            pass_count += 1
        else:
            print(f"FAIL {actual} (!!!) ({time_info})")
            fail_count += 1

    print()
    msg = f"Results: {pass_count} passed, {fail_count} failed"
    if outlier_count > 0:
        msg += f" ({outlier_count} timing outlier{'s' if outlier_count > 1 else ''})"
    print(msg)
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

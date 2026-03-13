#!/usr/bin/env python3
"""
Benchmark ASP (clingo) vs ZDD (botc-zdd-) for Night 1 info role counting.

Measures wall-clock time per scenario for each engine, plus ZDD node counts.
Reuses scenario definitions and helpers from validate_night_info.py.
"""

import time
import json
import subprocess
import sys
from pathlib import Path

# Reuse definitions from the validation script
sys.path.insert(0, str(Path(__file__).parent))
from validate_night_info import (
    SCENARIOS, PLAYERS, ZDD_REPO, ASP_ROOT,
    load_asp_program, count_asp_models, zdd_role_name,
)


def benchmark_asp(scenarios, base_program):
    """Time each ASP scenario individually. Returns list of (name, count, seconds)."""
    results = []
    for sc in scenarios:
        t0 = time.perf_counter()
        count = count_asp_models(sc, base_program)
        elapsed = time.perf_counter() - t0
        results.append((sc["name"], count, elapsed))
    return results


ZDD_BENCH_SCRIPT = """
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
  const t0 = process.hrtime.bigint();
  const result = buildNightInfoZDD(zdd, {
    numPlayers: config.player_count,
    seatRoles,
    selectedRoles: Object.values(config.seats),
    script: TROUBLE_BREWING,
  });
  const count = zdd.count(result.root);
  const elapsed = Number(process.hrtime.bigint() - t0) / 1e6;  // ms

  results[name] = {
    total: count,
    variableCount: result.variableCount,
    nodeCount: zdd.size,
    elapsed_ms: elapsed,
  };
}

// Memory usage
const mem = process.memoryUsage();
results["__memory__"] = {
  rss_mb: (mem.rss / 1048576).toFixed(1),
  heap_used_mb: (mem.heapUsed / 1048576).toFixed(1),
  heap_total_mb: (mem.heapTotal / 1048576).toFixed(1),
};

console.log(JSON.stringify(results));
"""


def benchmark_zdd(scenarios):
    """Run ZDD benchmarks. Returns dict of results per scenario."""
    zdd_scenarios = {}
    for sc in scenarios:
        zdd_seats = {}
        for seat, role in sc["seats"].items():
            zdd_seats[str(seat)] = zdd_role_name(role)
        zdd_scenarios[sc["name"]] = {
            "seats": zdd_seats,
            "player_count": sc["player_count"],
        }

    script = ZDD_BENCH_SCRIPT.replace("%SCENARIOS%", json.dumps(zdd_scenarios))
    t0 = time.perf_counter()
    result = subprocess.run(
        ["node", "-e", script],
        cwd=str(ZDD_REPO),
        capture_output=True,
        text=True,
    )
    total_zdd_time = time.perf_counter() - t0
    if result.returncode != 0:
        print(f"ZDD error: {result.stderr}", file=sys.stderr)
        sys.exit(1)
    data = json.loads(result.stdout)
    data["__total_wall__"] = total_zdd_time
    return data


def main():
    import resource

    print("=" * 78)
    print("Benchmark: ASP (clingo) vs ZDD (botc-zdd-) — Night 1 Info")
    print("=" * 78)

    base_program = load_asp_program()

    # --- ASP benchmarks ---
    print("\nASP (clingo) per-scenario timings:")
    mem_before = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    asp_total_t0 = time.perf_counter()
    asp_results = benchmark_asp(SCENARIOS, base_program)
    asp_total = time.perf_counter() - asp_total_t0
    mem_after = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss

    for name, count, elapsed in asp_results:
        print(f"  {name:55s}  {count:>8,}  {elapsed*1000:8.1f} ms")

    print(f"  {'TOTAL':55s}  {'':>8s}  {asp_total*1000:8.1f} ms")
    print(f"  Peak RSS: ~{mem_after / 1024:.1f} MB")

    # --- ZDD benchmarks ---
    zdd_available = (ZDD_REPO / "dist" / "zdd.js").exists()
    if not zdd_available:
        print(f"\nZDD repo not found at {ZDD_REPO}, skipping ZDD benchmarks")
        return

    print("\nZDD (botc-zdd-) per-scenario timings:")
    zdd_data = benchmark_zdd(SCENARIOS)
    zdd_mem = zdd_data.get("__memory__", {})
    zdd_wall = zdd_data.get("__total_wall__", 0)

    for sc in SCENARIOS:
        name = sc["name"]
        if name in zdd_data:
            d = zdd_data[name]
            print(f"  {name:55s}  {d['total']:>8,}  {d['elapsed_ms']:8.1f} ms"
                  f"  ({d['nodeCount']:,} nodes, {d['variableCount']} vars)")

    print(f"  {'TOTAL (incl. Node startup)':55s}  {'':>8s}  {zdd_wall*1000:8.1f} ms")
    if zdd_mem:
        print(f"  Node RSS: {zdd_mem['rss_mb']} MB, Heap used: {zdd_mem['heap_used_mb']} MB")

    # --- Summary ---
    print("\n" + "=" * 78)
    print("Per-scenario comparison (ms):")
    print(f"  {'Scenario':<55s}  {'ASP':>8s}  {'ZDD':>8s}  {'Ratio':>7s}")
    print(f"  {'-'*55}  {'-'*8}  {'-'*8}  {'-'*7}")
    for (name, count, asp_ms) in asp_results:
        if name in zdd_data:
            zdd_ms = zdd_data[name]["elapsed_ms"]
            ratio = (asp_ms * 1000) / zdd_ms if zdd_ms > 0 else float('inf')
            print(f"  {name:55s}  {asp_ms*1000:8.1f}  {zdd_ms:8.1f}  {ratio:6.1f}x")
    print("=" * 78)


if __name__ == "__main__":
    main()

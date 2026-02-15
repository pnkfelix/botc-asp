#!/usr/bin/env python3
"""Wrapper to run clingo with the nix-provided version.

Usage:
    ./run-clingo.py [clingo args...]
    ./run-clingo.py botc.lp tb.lp tb_tests/sat_night2_imp_kills.lp 0

Finds clingo by checking (in order):
  1. Already on PATH (e.g. direnv loaded)
  2. direnv export from .envrc/flake.nix
  3. Direct search in /nix/store for a clingo binary
"""

import os
import shutil
import subprocess
import sys
from pathlib import Path


def find_clingo():
    """Return the path to a clingo binary, or None."""

    # 1. Already on PATH?
    clingo = shutil.which("clingo")
    if clingo:
        return clingo

    # 2. Try loading direnv environment
    direnv = shutil.which("direnv")
    if not direnv:
        # Search nix store for direnv (targeted, not a full glob)
        nix_store = Path("/nix/store")
        if nix_store.is_dir():
            for entry in nix_store.iterdir():
                if "direnv" in entry.name:
                    candidate = entry / "bin" / "direnv"
                    if candidate.is_file():
                        direnv = str(candidate)
                        break

    if direnv:
        try:
            result = subprocess.run(
                [direnv, "export", "bash"],
                capture_output=True, text=True, timeout=30,
                cwd=Path(__file__).resolve().parent,
            )
            if result.returncode == 0 and result.stdout.strip():
                # Parse the export lines to find PATH updates
                env = os.environ.copy()
                for line in result.stdout.strip().split("\n"):
                    line = line.strip()
                    if line.startswith("export "):
                        line = line[len("export "):]
                    if "=" in line:
                        key, _, val = line.partition("=")
                        # Remove surrounding quotes
                        val = val.strip("'\"")
                        env[key] = val
                if "PATH" in env:
                    for d in env["PATH"].split(":"):
                        candidate = Path(d) / "clingo"
                        if candidate.is_file():
                            return str(candidate)
        except (subprocess.TimeoutExpired, OSError):
            pass

    # 3. Direct nix store search for clingo
    nix_store = Path("/nix/store")
    if nix_store.is_dir():
        import re
        best = None
        best_version = ()
        for entry in nix_store.iterdir():
            if "clingo" in entry.name and "doc" not in entry.name:
                candidate = entry / "bin" / "clingo"
                if candidate.is_file():
                    # Extract version tuple for comparison (e.g. "clingo-5.8.0" -> (5,8,0))
                    m = re.search(r"clingo-(\d+(?:\.\d+)*)", entry.name)
                    version = tuple(int(x) for x in m.group(1).split(".")) if m else ()
                    if best is None or version > best_version:
                        best = candidate
                        best_version = version
        if best:
            return str(best)

    return None


def main():
    clingo = find_clingo()
    if not clingo:
        print(
            "Error: clingo not found. Install via nix (nix develop) or your package manager.",
            file=sys.stderr,
        )
        sys.exit(1)

    os.execv(clingo, [clingo] + sys.argv[1:])


if __name__ == "__main__":
    main()

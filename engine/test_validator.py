#!/usr/bin/env python3
"""
Test script for the BotC engine validator.

Verifies that:
1. The validator can load ASP files
2. It can enumerate legal game setups
3. Basic queries work
"""

import sys
from pathlib import Path

# Add parent directory to path so we can import the engine
sys.path.insert(0, str(Path(__file__).parent.parent))

from engine import (
    Validator, GameConfig, GameState, Setup, Night, Day,
)


def test_enumerate_setups():
    """Test that we can enumerate legal role assignments."""
    print("=" * 60)
    print("Test: Enumerate legal setups for 5 players")
    print("=" * 60)

    # Path to ASP files (parent of engine directory)
    asp_path = Path(__file__).parent.parent

    # Create validator for Trouble Brewing
    validator = Validator(asp_path, script="tb")

    # Create a 5-player game config
    players = frozenset(["alice", "bob", "charlie", "diana", "eve"])
    config = GameConfig(script="trouble_brewing", player_names=players)

    # Enumerate some setups
    print(f"\nFinding legal setups for {len(players)} players...")
    setups = validator.enumerate_legal_setups(config, max_models=5)

    print(f"\nFound {len(setups)} legal setups:")
    for i, setup in enumerate(setups, 1):
        print(f"\n  Setup {i}:")
        for player, role in sorted(setup.items()):
            print(f"    {player}: {role}")

    assert len(setups) > 0, "Should find at least one legal setup"
    print("\n✓ Test passed!")
    return True


def test_types_to_asp():
    """Test that our types convert to ASP correctly."""
    print("\n" + "=" * 60)
    print("Test: Type to ASP conversion")
    print("=" * 60)

    # Test time types
    setup = Setup()
    assert setup.to_asp() == "setup", f"Expected 'setup', got '{setup.to_asp()}'"
    print(f"  Setup().to_asp() = '{setup.to_asp()}' ✓")

    night1 = Night(1)
    assert night1.to_asp() == "night(1,0,0)", f"Got '{night1.to_asp()}'"
    print(f"  Night(1).to_asp() = '{night1.to_asp()}' ✓")

    night2_r1 = Night(2, round=1, step=3)
    assert night2_r1.to_asp() == "night(2,1,3)", f"Got '{night2_r1.to_asp()}'"
    print(f"  Night(2,1,3).to_asp() = '{night2_r1.to_asp()}' ✓")

    # Test game state to ASP
    players = frozenset(["alice", "bob"])
    config = GameConfig(script="tb", player_names=players)
    state = GameState(config=config, time=Setup(), events=[])

    asp = state.to_asp()
    print(f"\n  GameState.to_asp():\n{asp}")

    assert "script(tb)." in asp
    assert "player(alice)." in asp or "player(bob)." in asp
    assert "current_time(setup)." in asp
    print("\n✓ Test passed!")
    return True


def test_validator_loads():
    """Test that the validator can load the base ASP program."""
    print("\n" + "=" * 60)
    print("Test: Validator loads ASP files")
    print("=" * 60)

    asp_path = Path(__file__).parent.parent
    validator = Validator(asp_path, script="tb")

    # This will load and cache the base program
    base_program = validator._get_base_program()

    print(f"\n  Loaded base program: {len(base_program)} characters")

    # Check that key components are present
    checks = [
        ("role definitions", "townsfolk" in base_program.lower()),
        ("demon role", "demon" in base_program.lower()),
        ("time handling", "night(" in base_program),
    ]

    for name, passed in checks:
        status = "✓" if passed else "✗"
        print(f"  {status} Contains {name}")
        if not passed:
            return False

    print("\n✓ Test passed!")
    return True


def main():
    """Run all tests."""
    print("\nBotC Engine Validator Tests")
    print("=" * 60)

    tests = [
        test_types_to_asp,
        test_validator_loads,
        test_enumerate_setups,
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            if test():
                passed += 1
            else:
                failed += 1
                print(f"\n✗ {test.__name__} failed")
        except Exception as e:
            failed += 1
            print(f"\n✗ {test.__name__} raised exception: {e}")
            import traceback
            traceback.print_exc()

    print("\n" + "=" * 60)
    print(f"Results: {passed} passed, {failed} failed")
    print("=" * 60)

    return failed == 0


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)

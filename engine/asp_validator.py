"""
ASP-based validator for Blood on the Clocktower.

This module provides single-transition validation using the real ASP
infrastructure (botc.lp + script files), ensuring all rule validation
happens in ASP rather than duplicated Python logic.

Two modes are supported:
- full-trace: Uses botc.lp, derives state via inertia from initial conditions.
              Slower but validates full game history consistency.
- incremental: Uses incremental.lp, accepts state as input facts.
               Much faster (40-60x), validates single action in isolation.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Mapping, Optional, List, Set, Tuple, Any, FrozenSet, Literal
from pathlib import Path
import time
from enum import Enum

import clingo


class ValidationMode(Enum):
    FULL_TRACE = "full-trace"
    INCREMENTAL = "incremental"


@dataclass
class AspValidationResult:
    """Result of an ASP validation query."""
    valid: bool
    elapsed_ms: float
    error: Optional[str] = None
    # For debugging/display
    details: Optional[dict] = None


@dataclass
class GameStateSnapshot:
    """
    A snapshot of game state at a specific time for ASP validation.

    This captures everything ASP needs to validate a single transition.
    """
    # Players and seating
    players: FrozenSet[str]  # Player names
    seating: Mapping[str, int]  # player -> chair number

    # Role assignments (initial, at time 0)
    assignments: Mapping[str, str]  # player -> role

    # What tokens players received (usually matches assignments)
    received: Mapping[str, str]  # player -> token

    # Bag contents
    bag: FrozenSet[str]  # Roles in the bag

    # Bluffs shown to demon
    bluffs: FrozenSet[str]  # 3 roles

    # Current time (which night/day we're on)
    current_night: int

    # Death history: who was executed on which day
    executions: Mapping[int, str]  # day -> player executed

    # Any additional facts to assert
    extra_facts: str = ""


@dataclass
class AspValidator:
    """
    Validates BotC transitions using ASP.

    Loads botc.lp + script files once, then validates individual
    transitions by asserting state + proposed action and checking SAT.

    Two modes are supported:
    - FULL_TRACE: Uses botc.lp, derives state via inertia from initial conditions.
                  Slower but validates full game history consistency.
    - INCREMENTAL: Uses incremental.lp, accepts state as input facts.
                   Much faster (40-60x), validates single action in isolation.
    """
    asp_path: Path
    script: str = "tb"
    mode: ValidationMode = ValidationMode.INCREMENTAL  # Default to fast mode

    # Cached file contents
    _base_program: str = field(default="", repr=False)
    _loaded: bool = field(default=False, repr=False)

    def _ensure_loaded(self) -> None:
        """Load ASP files if not already loaded."""
        if self._loaded:
            return

        # We don't cache the full program because clingo needs to reload files
        # Just verify they exist
        if self.mode == ValidationMode.INCREMENTAL:
            base_file = self.asp_path / "incremental.lp"
            if not base_file.exists():
                raise FileNotFoundError(f"Missing incremental.lp at {base_file}")
        else:
            base_file = self.asp_path / "botc.lp"
            if not base_file.exists():
                raise FileNotFoundError(f"Missing botc.lp at {base_file}")

        script = self.asp_path / f"{self.script}.lp"
        if not script.exists():
            raise FileNotFoundError(f"Missing script {self.script}.lp at {script}")

        self._loaded = True

    def _state_to_asp(self, state: GameStateSnapshot) -> str:
        """Convert a game state snapshot to ASP facts.

        Depending on mode:
        - FULL_TRACE: Uses assigned(0, ...) constraints and executions
        - INCREMENTAL: Uses inc_* predicates for current state

        We only constrain what's necessary, letting ASP derive the rest.
        """
        if self.mode == ValidationMode.INCREMENTAL:
            return self._state_to_asp_incremental(state)
        else:
            return self._state_to_asp_full_trace(state)

    def _state_to_asp_incremental(self, state: GameStateSnapshot) -> str:
        """Convert state to ASP facts for incremental mode.

        Uses inc_* predicates to provide current state directly.
        """
        lines = []

        # Player count
        lines.append(f"#const player_count = {len(state.players)}.")
        lines.append("")

        # Players and seating
        for player in sorted(state.players):
            chair = state.seating[player]
            lines.append(f"name({player}). chair({player}, {chair}).")

        lines.append("")

        # Current night (required for time modeling)
        lines.append(f"current_night({state.current_night}).")
        lines.append("")

        # Current role assignments (inc_role)
        for player, role in sorted(state.assignments.items()):
            lines.append(f"inc_role({player}, {role}).")

        lines.append("")

        # Who is alive (based on executions - anyone not executed is alive)
        executed_players = set(state.executions.values())
        for player in sorted(state.players):
            if player not in executed_players:
                lines.append(f"inc_alive({player}).")

        lines.append("")

        # Check if any minion is alive
        minion_roles = {"poisoner", "spy", "scarlet_woman", "baron"}  # TB minions
        for player, role in state.assignments.items():
            if role in minion_roles and player not in executed_players:
                lines.append("inc_minion_alive.")
                break

        if state.extra_facts:
            lines.append("")
            lines.append(state.extra_facts)

        return "\n".join(lines)

    def _state_to_asp_full_trace(self, state: GameStateSnapshot) -> str:
        """Convert state to ASP facts for full-trace mode.

        Uses assigned(0, ...) constraints and derives state via inertia.
        """
        lines = []

        # Player count
        lines.append(f"#const player_count = {len(state.players)}.")
        lines.append("")

        # Players and seating
        for player in sorted(state.players):
            chair = state.seating[player]
            lines.append(f"name({player}). chair({player}, {chair}).")

        lines.append("")

        # Request modeling of the current night
        if state.current_night > 1:
            lines.append(f"needs_night({state.current_night}).")
            lines.append("")

        # Role assignments (constrain rather than assert, to let ASP verify consistency)
        for player, role in sorted(state.assignments.items()):
            lines.append(f":- not assigned(0, {player}, {role}).")

        lines.append("")

        # Executions (who died when) - these are facts, not constraints
        for day, player in sorted(state.executions.items()):
            lines.append(f"executed({player}, {day}).")

        if state.extra_facts:
            lines.append("")
            lines.append(state.extra_facts)

        return "\n".join(lines)

    def validate_imp_kill(
        self,
        state: GameStateSnapshot,
        imp_player: str,
        target: str,
    ) -> AspValidationResult:
        """
        Validate an Imp kill action.

        Args:
            state: Current game state
            imp_player: The player who is the Imp
            target: Who the Imp wants to kill

        Returns:
            AspValidationResult indicating if the action is valid
        """
        self._ensure_loaded()

        # Build the constraint for this specific action
        # From base.lp: other_night_role_order(imp, 4)
        imp_order = 4  # Could query this, but for TB it's constant
        time_point = f"night({state.current_night}, {imp_order}, 2)"

        action_constraint = f":- not player_chooses(imp, {imp_player}, point({target}), {time_point})."

        # Full program
        state_asp = self._state_to_asp(state)
        program = f"""
{state_asp}

% === PROPOSED ACTION ===
{action_constraint}

#show player_chooses/4.
"""

        return self._run_validation(program)

    def validate_monk_protection(
        self,
        state: GameStateSnapshot,
        monk_player: str,
        target: str,
    ) -> AspValidationResult:
        """Validate a Monk protection action."""
        self._ensure_loaded()

        # From base.lp: other_night_role_order(monk, 2)
        monk_order = 2
        time_point = f"night({state.current_night}, {monk_order}, 2)"

        action_constraint = f":- not player_chooses(monk, {monk_player}, point({target}), {time_point})."

        state_asp = self._state_to_asp(state)
        program = f"""
{state_asp}

{action_constraint}

#show player_chooses/4.
"""

        return self._run_validation(program)

    def validate_poisoner_poison(
        self,
        state: GameStateSnapshot,
        poisoner_player: str,
        target: str,
    ) -> AspValidationResult:
        """Validate a Poisoner poisoning action."""
        self._ensure_loaded()

        # From base.lp: other_night_role_order(poisoner, 1) (also first_night)
        poisoner_order = 1
        time_point = f"night({state.current_night}, {poisoner_order}, 2)"

        action_constraint = f":- not player_chooses(poisoner, {poisoner_player}, point({target}), {time_point})."

        state_asp = self._state_to_asp(state)
        program = f"""
{state_asp}

{action_constraint}

#show player_chooses/4.
"""

        return self._run_validation(program)

    def enumerate_valid_targets(
        self,
        state: GameStateSnapshot,
        role: str,
        player: str,
    ) -> List[str]:
        """
        Enumerate all valid targets for a role's ability.

        Returns a list of player names that are valid targets.
        """
        self._ensure_loaded()

        # Get role order from script
        role_orders = {
            "imp": 4,
            "monk": 2,
            "poisoner": 1,
            # Add more as needed
        }

        order = role_orders.get(role)
        if order is None:
            return []

        time_point = f"night({state.current_night}, {order}, 2)"

        state_asp = self._state_to_asp(state)
        program = f"""
{state_asp}

% Show what targets this role can choose
#show player_chooses({role}, {player}, point(X), {time_point}) : player_chooses({role}, {player}, point(X), {time_point}).
"""

        start = time.perf_counter()

        ctl = clingo.Control(["--models=0"])  # Enumerate all
        ctl.add("base", [], program)

        # Load the appropriate base file based on mode
        if self.mode == ValidationMode.INCREMENTAL:
            ctl.load(str(self.asp_path / "incremental.lp"))
        else:
            ctl.load(str(self.asp_path / "botc.lp"))

        ctl.load(str(self.asp_path / f"{self.script}.lp"))

        ctl.ground([("base", [])])

        targets = set()
        def on_model(m):
            for atom in m.symbols(shown=True):
                atom_str = str(atom)
                # Extract target from player_chooses(..., point(X), ...)
                if "point(" in atom_str:
                    import re
                    match = re.search(r'point\((\w+)\)', atom_str)
                    if match:
                        targets.add(match.group(1))

        ctl.solve(on_model=on_model)

        return sorted(targets)

    def _run_validation(self, program: str) -> AspValidationResult:
        """Run a validation query and return the result."""
        start = time.perf_counter()

        try:
            ctl = clingo.Control(["--models=1"])
            ctl.add("base", [], program)

            # Load the appropriate base file based on mode
            if self.mode == ValidationMode.INCREMENTAL:
                ctl.load(str(self.asp_path / "incremental.lp"))
            else:
                ctl.load(str(self.asp_path / "botc.lp"))

            ctl.load(str(self.asp_path / f"{self.script}.lp"))

            ctl.ground([("base", [])])

            result = []
            def on_model(m):
                result.append([str(a) for a in m.symbols(shown=True)])

            ctl.solve(on_model=on_model)

            elapsed = (time.perf_counter() - start) * 1000

            if result:
                return AspValidationResult(
                    valid=True,
                    elapsed_ms=elapsed,
                    details={"validated_atoms": result[0], "mode": self.mode.value}
                )
            else:
                return AspValidationResult(
                    valid=False,
                    elapsed_ms=elapsed,
                    error="UNSAT - action violates game constraints"
                )

        except Exception as e:
            elapsed = (time.perf_counter() - start) * 1000
            return AspValidationResult(
                valid=False,
                elapsed_ms=elapsed,
                error=str(e)
            )


def demo():
    """Demonstrate the ASP validator in both modes."""
    asp_path = Path(__file__).parent.parent

    # Create a game state
    players = frozenset(["alice", "bob", "charlie", "diana", "eve"])
    state = GameStateSnapshot(
        players=players,
        seating={"alice": 0, "bob": 1, "charlie": 2, "diana": 3, "eve": 4},
        assignments={
            "alice": "washerwoman",
            "bob": "empath",
            "charlie": "monk",
            "diana": "imp",
            "eve": "poisoner",
        },
        received={
            "alice": "washerwoman",
            "bob": "empath",
            "charlie": "monk",
            "diana": "imp",
            "eve": "poisoner",
        },
        bag=frozenset(["washerwoman", "empath", "monk", "imp", "poisoner"]),
        bluffs=frozenset(["chef", "fortune_teller", "butler"]),
        current_night=2,
        executions={},
    )

    state_no_minion = GameStateSnapshot(
        players=players,
        seating={"alice": 0, "bob": 1, "charlie": 2, "diana": 3, "eve": 4},
        assignments={
            "alice": "washerwoman",
            "bob": "empath",
            "charlie": "monk",
            "diana": "imp",
            "eve": "poisoner",
        },
        received={
            "alice": "washerwoman",
            "bob": "empath",
            "charlie": "monk",
            "diana": "imp",
            "eve": "poisoner",
        },
        bag=frozenset(["washerwoman", "empath", "monk", "imp", "poisoner"]),
        bluffs=frozenset(["chef", "fortune_teller", "butler"]),
        current_night=2,
        executions={1: "eve"},  # Eve was executed day 1
    )

    print("=" * 60)
    print("ASP Validator Demo - Comparing INCREMENTAL vs FULL_TRACE")
    print("=" * 60)

    for mode in [ValidationMode.INCREMENTAL, ValidationMode.FULL_TRACE]:
        print(f"\n{'=' * 60}")
        print(f"Mode: {mode.value}")
        print("=" * 60)

        validator = AspValidator(asp_path, script="tb", mode=mode)

        # Test 1: Valid Imp kill
        print("\n1. Validate: diana (Imp) kills alice")
        result = validator.validate_imp_kill(state, "diana", "alice")
        print(f"   Valid: {result.valid}")
        print(f"   Time: {result.elapsed_ms:.2f}ms")

        # Test 2: Valid starpass (self-kill with minion alive)
        print("\n2. Validate: diana (Imp) starpass (self-kill)")
        result = validator.validate_imp_kill(state, "diana", "diana")
        print(f"   Valid: {result.valid} (should be True - minion eve is alive)")
        print(f"   Time: {result.elapsed_ms:.2f}ms")

        # Test 3: Invalid starpass (no minions alive)
        print("\n3. Validate: diana starpass with no minions")
        result = validator.validate_imp_kill(state_no_minion, "diana", "diana")
        print(f"   Valid: {result.valid} (should be False - no minions alive)")
        print(f"   Time: {result.elapsed_ms:.2f}ms")

        # Test 4: Enumerate valid Imp targets
        print("\n4. Enumerate valid Imp targets for diana")
        targets = validator.enumerate_valid_targets(state, "imp", "diana")
        print(f"   Valid targets: {targets}")


if __name__ == "__main__":
    demo()

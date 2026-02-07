"""
Validator: Interface between Python engine and Clingo ASP solver.

This module handles:
- Loading ASP programs (base rules + script + role definitions)
- Running validation queries
- Extracting results from answer sets

The validator doesn't understand roles - it just runs ASP and parses results.
"""

from __future__ import annotations
import os
import re
from pathlib import Path
from dataclasses import dataclass
from typing import Optional, Sequence, FrozenSet, Mapping, Any, List, Set

import clingo

from .types import (
    GameState, GameTransition, ValidationResult,
    Time, Setup, Night, Day, GameConfig
)


@dataclass
class AspResult:
    """Raw result from running clingo."""
    satisfiable: bool
    models: List[Set[str]]  # List of answer sets (each is set of atoms)
    statistics: Optional[dict] = None


class Validator:
    """
    Validates game transitions using ASP.

    The validator loads the base game rules and script-specific role
    definitions, then runs queries to validate proposed transitions.
    """

    def __init__(self, asp_base_path: Path, script: str = "tb"):
        """
        Initialize validator with path to ASP files.

        Args:
            asp_base_path: Path to directory containing .lp files
            script: Script identifier (e.g., "tb" for Trouble Brewing)
        """
        self.asp_base_path = asp_base_path
        self.script = script
        self._cached_base_program: Optional[str] = None

    def _load_file(self, filename: str) -> str:
        """Load an ASP file."""
        path = self.asp_base_path / filename
        if path.exists():
            return path.read_text()
        raise FileNotFoundError(f"ASP file not found: {path}")

    def _get_base_program(self) -> str:
        """
        Load and cache the base ASP program.

        This includes:
        - Core game mechanics (botc.lp)
        - Script registration (e.g., tb.lp)
        - Role definitions (from roles/ directory)
        - Query layer (engine/query_layer.lp)
        """
        if self._cached_base_program is not None:
            return self._cached_base_program

        parts = []

        # Core mechanics
        core_files = ["types.lp", "botc.lp", "night_order.lp", "game_end.lp"]
        for f in core_files:
            try:
                parts.append(f"% === {f} ===")
                parts.append(self._load_file(f))
            except FileNotFoundError:
                pass  # Some files may be optional

        # Script file
        script_file = f"{self.script}.lp"
        try:
            parts.append(f"% === {script_file} ===")
            parts.append(self._load_file(script_file))
        except FileNotFoundError:
            raise ValueError(f"Unknown script: {self.script}")

        # Role definitions - recursively load from roles/{script}/
        roles_dir = self.asp_base_path / "roles" / self.script
        if roles_dir.exists():
            for lp_file in sorted(roles_dir.rglob("*.lp")):
                rel_path = lp_file.relative_to(self.asp_base_path)
                parts.append(f"% === {rel_path} ===")
                parts.append(lp_file.read_text())

        # Query layer
        query_layer_path = self.asp_base_path / "engine" / "query_layer.lp"
        if query_layer_path.exists():
            parts.append("% === engine/query_layer.lp ===")
            parts.append(query_layer_path.read_text())

        self._cached_base_program = "\n\n".join(parts)
        return self._cached_base_program

    def _run_clingo(
        self,
        program: str,
        num_models: int = 1,
        timeout: Optional[float] = None
    ) -> AspResult:
        """
        Run clingo on a program and return results.

        Args:
            program: The complete ASP program
            num_models: Max number of models to find (0 = all)
            timeout: Optional timeout in seconds
        """
        ctl = clingo.Control([f"-n{num_models}"])

        # Add the program
        ctl.add("base", [], program)

        # Ground
        ctl.ground([("base", [])])

        # Solve
        models: List[Set[str]] = []

        def on_model(model: clingo.Model) -> bool:
            atoms = set(str(atom) for atom in model.symbols(shown=True))
            models.append(atoms)
            return True  # Continue searching

        # Handle timeout if specified
        if timeout:
            result = ctl.solve(on_model=on_model, async_=True)
            finished = result.wait(timeout)
            if not finished:
                result.cancel()
        else:
            ctl.solve(on_model=on_model)

        satisfiable = len(models) > 0

        return AspResult(
            satisfiable=satisfiable,
            models=models,
            statistics=ctl.statistics
        )

    def validate_transition(
        self,
        state: GameState,
        transition: GameTransition
    ) -> ValidationResult:
        """
        Validate a proposed transition.

        Returns ValidationResult indicating whether the transition is legal.
        """
        # Build the complete program
        parts = [
            self._get_base_program(),
            "% === Current game state ===",
            state.to_asp(),
            "% === Proposed transition ===",
            transition.to_asp(),
            "% === Enable validation check ===",
            "check_transition_validity."
        ]
        program = "\n\n".join(parts)

        # Run clingo
        result = self._run_clingo(program, num_models=1)

        if result.satisfiable:
            return ValidationResult(
                valid=True,
                new_state=self._compute_new_state(state, transition, result),
                asp_program=program,
                asp_result=str(result.models[0]) if result.models else None
            )
        else:
            # Try to extract the reason for failure
            error = self._extract_error(program)
            return ValidationResult(
                valid=False,
                error=error or "Transition invalid (unsatisfiable)",
                asp_program=program
            )

    def _extract_error(self, program: str) -> Optional[str]:
        """
        Try to extract why a transition was invalid.

        Runs the program without the validity constraint to see what
        invalidity reasons are derived.
        """
        # Remove the constraint and add show for invalid reasons
        diagnostic_program = program.replace(
            "check_transition_validity.",
            "% check_transition_validity.  % disabled for diagnostics"
        )
        diagnostic_program += "\n#show proposed_ability_invalid/1."

        result = self._run_clingo(diagnostic_program, num_models=1)
        if result.satisfiable and result.models:
            model = result.models[0]
            invalids = [a for a in model if a.startswith("proposed_ability_invalid")]
            if invalids:
                return f"Invalid: {', '.join(invalids)}"
        return None

    def _compute_new_state(
        self,
        state: GameState,
        transition: GameTransition,
        result: AspResult
    ) -> GameState:
        """
        Compute the new game state after a valid transition.

        This extracts derived facts from the answer set.
        """
        # For now, just return the state unchanged
        # TODO: Extract events and state changes from the model
        return state

    def query_available_abilities(
        self,
        state: GameState,
        player: str
    ) -> Sequence[str]:
        """
        Query what abilities a player can use right now.

        Returns list of ability names.
        """
        program = "\n\n".join([
            self._get_base_program(),
            "% === Current game state ===",
            state.to_asp(),
            f"% === Query for player {player} ===",
            f"#show available_ability({player}, A, T) : available_ability({player}, A, T)."
        ])

        result = self._run_clingo(program, num_models=1)

        if result.satisfiable and result.models:
            model = result.models[0]
            # Parse ability names from atoms like "available_ability(alice,kill,night(1,0,0))"
            abilities = []
            pattern = re.compile(rf"available_ability\({player},(\w+),")
            for atom in model:
                m = pattern.match(atom)
                if m:
                    abilities.append(m.group(1))
            return abilities
        return []

    def query_valid_targets(
        self,
        state: GameState,
        player: str,
        ability: str
    ) -> Sequence[str]:
        """
        Query valid targets for an ability.

        Returns list of player names that are valid targets.
        """
        program = "\n\n".join([
            self._get_base_program(),
            "% === Current game state ===",
            state.to_asp(),
            f"% === Query targets for {player}'s {ability} ===",
            f"#show valid_target({player}, {ability}, Target, T) : valid_target({player}, {ability}, Target, T)."
        ])

        result = self._run_clingo(program, num_models=1)

        if result.satisfiable and result.models:
            model = result.models[0]
            targets = []
            pattern = re.compile(rf"valid_target\({player},{ability},(\w+),")
            for atom in model:
                m = pattern.match(atom)
                if m:
                    targets.append(m.group(1))
            return targets
        return []

    def query_game_over(self, state: GameState) -> Optional[str]:
        """
        Check if the game is over.

        Returns "good", "evil", or None (game continues).
        """
        program = "\n\n".join([
            self._get_base_program(),
            "% === Current game state ===",
            state.to_asp(),
            "#show good_wins/1.",
            "#show evil_wins/1."
        ])

        result = self._run_clingo(program, num_models=1)

        if result.satisfiable and result.models:
            model = result.models[0]
            for atom in model:
                if atom.startswith("good_wins"):
                    return "good"
                if atom.startswith("evil_wins"):
                    return "evil"
        return None

    def enumerate_legal_setups(
        self,
        config: GameConfig,
        max_models: int = 10
    ) -> Sequence[Mapping[str, str]]:
        """
        Enumerate possible legal role assignments for a game.

        Returns list of dicts mapping player -> role.
        """
        # Build program with setup constraints
        # The existing ASP expects name/1, chair/2, and #const player_count
        player_list = sorted(config.player_names)
        num_players = len(player_list)

        # Generate name facts
        names_str = "; ".join(player_list)
        name_facts = f"name({names_str})."

        # Generate chair facts (seating order)
        chair_facts = "\n".join(
            f"chair({p}, {i})." for i, p in enumerate(player_list)
        )

        program = "\n\n".join([
            self._get_base_program(),
            f"% === Players ({num_players}) ===",
            f"#const player_count = {num_players}.",
            name_facts,
            chair_facts,
            "% === Only need setup/night1 for role assignment ===",
            "needs_night(1).",
            "% === Find valid assignments ===",
            "#show assigned/3."
        ])

        result = self._run_clingo(program, num_models=max_models)

        setups = []
        if result.satisfiable:
            for model in result.models:
                assignment = {}
                # assigned/3 is assigned(TimeIndex, Player, Role)
                # We want the initial assignment at time 0
                pattern = re.compile(r"assigned\(0,(\w+),(\w+)\)")
                for atom in model:
                    m = pattern.match(atom)
                    if m:
                        player, role = m.groups()
                        assignment[player] = role
                if assignment:
                    setups.append(assignment)
        return setups

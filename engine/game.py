"""
Game orchestrator for Blood on the Clocktower.

This module manages the game loop, coordinating between:
- The validator (ASP-based rule enforcement)
- Player agents (making decisions)
- Storyteller agent (managing game flow)

The orchestrator doesn't understand roles - it asks ASP what needs to happen
and who needs to make decisions.
"""

from __future__ import annotations
import asyncio
from dataclasses import dataclass, field
from typing import Mapping, Sequence, Optional, List, Dict, Any, FrozenSet
from pathlib import Path

from .types import (
    GameConfig, GameState, GameTransition,
    Time, Setup, Night, Day,
    AssignRoleTransition, UseAbilityTransition,
    NominateTransition, CastVoteTransition,
    ValidationResult
)
from .agent import (
    PlayerAgent, StorytellerAgent,
    PlayerView, StorytellerView, PublicGameView, ReceivedInfo,
    DecisionType, DecisionRequest, Decision,
    RandomPlayerAgent, RandomStorytellerAgent
)
from .validator import Validator
from .trace import GameTrace, InteractionType


@dataclass
class GameOrchestrator:
    """
    Orchestrates a game of Blood on the Clocktower.

    The orchestrator:
    1. Sets up the game (bag, role assignments)
    2. Runs the game loop (nights and days)
    3. Handles agent decisions
    4. Validates all transitions through ASP
    """
    config: GameConfig
    validator: Validator
    player_agents: Mapping[str, PlayerAgent]
    storyteller: StorytellerAgent

    # Internal state
    _state: Optional[GameState] = None
    _role_assignments: Dict[str, str] = field(default_factory=dict)
    _dead_players: set = field(default_factory=set)
    _ghost_votes_used: set = field(default_factory=set)
    _protected_tonight: Optional[str] = field(default=None)
    _poisoned_player: Optional[str] = field(default=None)
    _pending_kill: Optional[str] = field(default=None)
    _log: List[str] = field(default_factory=list)
    _trace: GameTrace = field(default_factory=GameTrace)

    @property
    def trace(self) -> GameTrace:
        """Access the game trace for rendering."""
        return self._trace

    def log(self, message: str) -> None:
        """Add a message to the game log."""
        self._log.append(message)
        print(f"[GAME] {message}")

    async def setup_game(self) -> GameState:
        """
        Set up the game: choose roles and assign them to players.

        Returns the initial game state.
        """
        self.log(f"Setting up game with {len(self.config.player_names)} players")
        self.log(f"Script: {self.config.script}")

        # Ask the validator to enumerate possible setups
        setups = self.validator.enumerate_legal_setups(self.config, max_models=1)

        if not setups:
            raise ValueError("No valid role assignments found for this configuration")

        # For now, just use the first valid setup
        # In a real game, the storyteller would choose
        assignment = setups[0]
        self._role_assignments = dict(assignment)

        self.log("Role assignments:")
        for player, role in sorted(assignment.items()):
            self.log(f"  {player}: {role}")

        # Create initial game state
        self._state = GameState(
            config=self.config,
            time=Night(1),  # Game starts at Night 1
            events=[]
        )

        # Notify players of their roles
        for player_name, agent in self.player_agents.items():
            role = self._role_assignments[player_name]
            alignment = self._get_alignment(role)

            view = self._make_player_view(player_name)
            await agent.observe(view, f"You are the {role} ({alignment})")

            # Evil players learn their teammates
            if alignment == "evil":
                teammates = [
                    p for p, r in self._role_assignments.items()
                    if p != player_name and self._get_alignment(r) == "evil"
                ]
                if teammates:
                    await agent.observe(view, f"Your evil teammates: {', '.join(teammates)}")

        return self._state

    def _get_alignment(self, role: str) -> str:
        """Get alignment for a role (simplified - should come from ASP)."""
        evil_roles = {"imp", "poisoner", "spy", "baron", "scarlet_woman"}
        return "evil" if role in evil_roles else "good"

    def _get_alive_players(self) -> FrozenSet[str]:
        """Get set of alive players."""
        return frozenset(p for p in self.config.player_names if p not in self._dead_players)

    def _make_public_view(self) -> PublicGameView:
        """Create the public game view."""
        alive = self._get_alive_players()
        return PublicGameView(
            time=self._state.time,
            players=self.config.player_names,
            alive=alive,
            dead=self.config.player_names - alive,
            ghost_votes_used=frozenset(),
            public_events=tuple(self._log[-10:])  # Last 10 events
        )

    def _make_player_view(self, player_name: str) -> PlayerView:
        """Create a view for a specific player."""
        public = self._make_public_view()
        role = self._role_assignments.get(player_name, "unknown")
        alignment = self._get_alignment(role)

        # Get known teammates for evil players
        known_teammates = None
        if alignment == "evil":
            known_teammates = frozenset(
                p for p, r in self._role_assignments.items()
                if p != player_name and self._get_alignment(r) == "evil"
            )

        return PlayerView(
            public=public,
            my_name=player_name,
            my_role=role,
            my_alignment=alignment,
            known_teammates=known_teammates
        )

    def _make_storyteller_view(self) -> StorytellerView:
        """Create the storyteller's view."""
        return StorytellerView(
            state=self._state,
            role_assignments=dict(self._role_assignments),
            statuses={}  # TODO: Get from ASP
        )

    async def run_night(self, night_number: int) -> Optional[str]:
        """
        Run a night phase.

        Returns the player killed tonight, or None.
        """
        self.log(f"\n{'='*50}")
        self.log(f"NIGHT {night_number}")
        self.log(f"{'='*50}")

        self._trace.set_phase(f"night_{night_number}")

        # Reset nightly state
        self._protected_tonight = None
        self._pending_kill = None

        # Clear previous poison (poisoner picks new target each night)
        self._poisoned_player = None

        alive = self._get_alive_players()

        # Process in night order:
        # 1. Poisoner (if alive)
        # 2. Monk (if alive, not night 1)
        # 3. Imp (demon kill)

        # --- Poisoner ---
        for player_name in alive:
            role = self._role_assignments.get(player_name)
            if role == "poisoner":
                agent = self.player_agents[player_name]
                view = self._make_player_view(player_name)
                targets = [p for p in alive if p != player_name]
                if targets:
                    self._trace.storyteller_to_player(
                        player_name, InteractionType.WAKE,
                        "Poisoner, wake. Choose someone to poison."
                    )
                    request = DecisionRequest(
                        decision_type=DecisionType.USE_ABILITY,
                        context={"ability": "poison", "valid_targets": targets},
                        valid_options=targets,
                        prompt="Choose a player to poison:"
                    )
                    decision = await agent.decide(view, request)
                    self._poisoned_player = decision.choice
                    self._trace.player_to_storyteller(
                        player_name, f"points at {self._poisoned_player}",
                        details={"ability": "poison", "target": self._poisoned_player}
                    )
                    self.log(f"  {player_name} (Poisoner) poisons {self._poisoned_player}")

        # --- Monk (not night 1) ---
        if night_number > 1:
            for player_name in alive:
                role = self._role_assignments.get(player_name)
                if role == "monk":
                    # Check if monk is poisoned (ability fails)
                    if player_name == self._poisoned_player:
                        self.log(f"  {player_name} (Monk) is poisoned - protection fails")
                        continue

                    agent = self.player_agents[player_name]
                    view = self._make_player_view(player_name)
                    targets = [p for p in alive if p != player_name]
                    if targets:
                        self._trace.storyteller_to_player(
                            player_name, InteractionType.WAKE,
                            "Monk, wake. Choose someone to protect."
                        )
                        request = DecisionRequest(
                            decision_type=DecisionType.USE_ABILITY,
                            context={"ability": "protect", "valid_targets": targets},
                            valid_options=targets,
                            prompt="Choose a player to protect:"
                        )
                        decision = await agent.decide(view, request)
                        self._protected_tonight = decision.choice
                        self._trace.player_to_storyteller(
                            player_name, f"points at {self._protected_tonight}",
                            details={"ability": "protect", "target": self._protected_tonight}
                        )
                        self.log(f"  {player_name} (Monk) protects {self._protected_tonight}")

        # --- Imp (demon kill) ---
        for player_name in alive:
            role = self._role_assignments.get(player_name)
            if role == "imp":
                agent = self.player_agents[player_name]
                view = self._make_player_view(player_name)
                targets = [p for p in alive if p != player_name]
                if targets:
                    self._trace.storyteller_to_player(
                        player_name, InteractionType.WAKE,
                        "Demon, wake. Choose someone to kill."
                    )
                    request = DecisionRequest(
                        decision_type=DecisionType.USE_ABILITY,
                        context={"ability": "kill", "valid_targets": targets},
                        valid_options=targets,
                        prompt="Choose a player to kill tonight:"
                    )
                    decision = await agent.decide(view, request)
                    self._pending_kill = decision.choice
                    self._trace.player_to_storyteller(
                        player_name, f"points at {self._pending_kill}",
                        details={"ability": "kill", "target": self._pending_kill}
                    )
                    self.log(f"  {player_name} (Imp) targets {self._pending_kill}")

        # --- Resolve night kill ---
        killed_tonight = None
        if self._pending_kill:
            if self._pending_kill == self._protected_tonight:
                self.log(f"  {self._pending_kill} was protected by the Monk!")
            else:
                killed_tonight = self._pending_kill
                self._dead_players.add(killed_tonight)
                self.log(f"  {killed_tonight} dies in the night!")

        return killed_tonight

    async def run_day(self, day_number: int, night_death: Optional[str] = None) -> Optional[str]:
        """
        Run a day phase.

        Returns the name of the executed player, or None if no execution.
        """
        self.log(f"\n{'='*50}")
        self.log(f"DAY {day_number}")
        self.log(f"{'='*50}")

        self._trace.set_phase(f"day_{day_number}")

        # Announce night death
        if night_death:
            self._trace.public_announcement(
                f"{night_death} was found dead!",
                details={"death": night_death, "cause": "night_kill"}
            )
            self.log(f"  ** {night_death} was found dead! **")

        alive = list(self._get_alive_players())
        executed = None

        # Simplified day: each alive player can nominate once
        nominations = []

        for player_name in alive:
            agent = self.player_agents[player_name]
            view = self._make_player_view(player_name)

            # Can only nominate alive players
            nominatable = [p for p in alive if p != player_name]
            request = DecisionRequest(
                decision_type=DecisionType.NOMINATE,
                context={},
                valid_options=["pass"] + nominatable,
                prompt="Nominate a player (or 'pass'):"
            )
            decision = await agent.decide(view, request)

            if decision.choice != "pass" and decision.choice in nominatable:
                nominee = decision.choice
                self._trace.nomination(player_name, nominee)
                self.log(f"  {player_name} nominates {nominee}")
                nominations.append((player_name, nominee))

        # Vote on nominations
        for nominator, nominee in nominations:
            # Skip if nominee is no longer alive (edge case)
            if nominee not in alive:
                continue

            self.log(f"\n  Voting on {nominee} (nominated by {nominator})")

            votes_for = []

            # Alive players vote
            for voter in alive:
                agent = self.player_agents[voter]
                view = self._make_player_view(voter)

                request = DecisionRequest(
                    decision_type=DecisionType.VOTE,
                    context={"nominee": nominee, "nominator": nominator},
                    valid_options=[True, False],
                    prompt=f"Vote to execute {nominee}? (True/False)"
                )
                decision = await agent.decide(view, request)

                self._trace.vote(voter, nominee, decision.choice, is_ghost=False)
                if decision.choice:
                    votes_for.append(voter)

            # Dead players with ghost votes can vote too
            for voter in self._dead_players:
                if voter not in self._ghost_votes_used:
                    agent = self.player_agents[voter]
                    view = self._make_player_view(voter)

                    request = DecisionRequest(
                        decision_type=DecisionType.VOTE,
                        context={"nominee": nominee, "nominator": nominator, "ghost_vote": True},
                        valid_options=[True, False],
                        prompt=f"Use your ghost vote to execute {nominee}? (True/False)"
                    )
                    decision = await agent.decide(view, request)

                    self._trace.vote(voter, nominee, decision.choice, is_ghost=True)
                    if decision.choice:
                        votes_for.append(f"{voter}👻")
                        self._ghost_votes_used.add(voter)

            total_voters = len(alive) + len(self._dead_players - self._ghost_votes_used)
            self.log(f"  Votes: {len(votes_for)}/{total_voters} ({', '.join(votes_for)})")

            # Majority of alive players needed (ghost votes count)
            if len(votes_for) > len(alive) // 2:
                executed = nominee
                self._dead_players.add(executed)
                self._trace.public_announcement(
                    f"{nominee} is executed!",
                    details={"execution": nominee, "votes": len(votes_for)}
                )
                self.log(f"  ** {nominee} is executed! **")
                break

        if not executed:
            self.log("  No execution today")

        return executed

    def _get_demon(self) -> Optional[str]:
        """Get the current (alive) demon player."""
        for player, role in self._role_assignments.items():
            if role == "imp" and player not in self._dead_players:
                return player
        return None

    async def run_game(self, max_days: int = 5) -> str:
        """
        Run a complete game.

        Returns "good" or "evil" indicating the winner.
        """
        # Setup
        await self.setup_game()

        winner = None

        for day in range(1, max_days + 1):
            # Night phase
            night_death = await self.run_night(day)

            # Check if demon was killed at night (shouldn't happen normally, but edge cases)
            demon = self._get_demon()
            if demon is None:
                winner = "good"
                self.log(f"\n*** GOOD WINS! The demon is dead! ***")
                break

            # Day phase
            executed = await self.run_day(day, night_death)

            # Check win conditions after execution
            if executed:
                demon = self._get_demon()
                if demon is None:
                    winner = "good"
                    self.log(f"\n*** GOOD WINS! The demon ({executed}) was executed! ***")
                    break

            # Check if evil wins (2 or fewer players alive)
            alive_count = len(self._get_alive_players())
            if alive_count <= 2:
                winner = "evil"
                self.log(f"\n*** EVIL WINS! Only {alive_count} players remain! ***")
                break

        if not winner:
            self.log("\n*** Game ended without clear winner (max days reached) ***")
            winner = "evil"  # Default to evil if game drags

        return winner


async def run_simple_game(
    player_names: Sequence[str],
    asp_path: Path,
    script: str = "tb",
    seed: Optional[int] = None
) -> str:
    """
    Run a simple game with random agents.

    Returns the winner ("good" or "evil").
    """
    # Create config
    config = GameConfig(
        script=script,
        player_names=frozenset(player_names)
    )

    # Create validator
    validator = Validator(asp_path, script=script)

    # Create random agents
    player_agents = {
        name: RandomPlayerAgent(name, seed=seed)
        for name in player_names
    }
    storyteller = RandomStorytellerAgent(seed=seed)

    # Create and run game
    game = GameOrchestrator(
        config=config,
        validator=validator,
        player_agents=player_agents,
        storyteller=storyteller
    )

    return await game.run_game()


# Convenience function for synchronous usage
def run_simple_game_sync(
    player_names: Sequence[str],
    asp_path: Path,
    script: str = "tb",
    seed: Optional[int] = None
) -> str:
    """Synchronous wrapper for run_simple_game."""
    return asyncio.run(run_simple_game(player_names, asp_path, script, seed))

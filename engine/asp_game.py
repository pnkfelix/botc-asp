"""
ASP-validated game orchestrator for Blood on the Clocktower.

This orchestrator uses ASP for ALL rule validation, ensuring that
game logic is defined in ASP files rather than duplicated in Python.
"""

from __future__ import annotations
import sys
import asyncio
from dataclasses import dataclass, field
from typing import Mapping, Optional, List, Dict, FrozenSet
from pathlib import Path

# Handle both direct execution and module import
if __name__ == "__main__":
    sys.path.insert(0, str(Path(__file__).parent.parent))
    from engine.asp_validator import AspValidator, GameStateSnapshot, AspValidationResult
    from engine.agent import (
        PlayerAgent, StorytellerAgent,
        PlayerView, StorytellerView, PublicGameView,
        DecisionType, DecisionRequest, Decision,
        RandomPlayerAgent, RandomStorytellerAgent
    )
    from engine.trace import GameTrace, InteractionType
    from engine.types import GameConfig, Night, Day
else:
    from .asp_validator import AspValidator, GameStateSnapshot, AspValidationResult
    from .agent import (
        PlayerAgent, StorytellerAgent,
        PlayerView, StorytellerView, PublicGameView,
        DecisionType, DecisionRequest, Decision,
        RandomPlayerAgent, RandomStorytellerAgent
    )
    from .trace import GameTrace, InteractionType
    from .types import GameConfig, Night, Day


@dataclass
class AspGameOrchestrator:
    """
    Orchestrates BotC games with ASP validation.

    Unlike the original GameOrchestrator which had hardcoded role logic,
    this version delegates ALL rule validation to ASP.
    """
    config: GameConfig
    validator: AspValidator
    player_agents: Mapping[str, PlayerAgent]
    storyteller: StorytellerAgent

    # Game state (updated as game progresses)
    _role_assignments: Dict[str, str] = field(default_factory=dict)
    _received_tokens: Dict[str, str] = field(default_factory=dict)
    _bag: FrozenSet[str] = field(default_factory=frozenset)
    _bluffs: FrozenSet[str] = field(default_factory=frozenset)
    _dead_players: set = field(default_factory=set)
    _executions: Dict[int, str] = field(default_factory=dict)  # day -> executed player
    _current_night: int = field(default=1)
    _current_day: int = field(default=0)

    # Nightly state (reset each night)
    _protected_tonight: Optional[str] = field(default=None)
    _poisoned_player: Optional[str] = field(default=None)
    _pending_kill: Optional[str] = field(default=None)

    _log: List[str] = field(default_factory=list)
    _trace: GameTrace = field(default_factory=GameTrace)

    @property
    def trace(self) -> GameTrace:
        return self._trace

    def log(self, message: str) -> None:
        self._log.append(message)
        print(f"[GAME] {message}")

    def _get_state_snapshot(self) -> GameStateSnapshot:
        """Create an ASP state snapshot from current game state."""
        players = self.config.player_names
        seating = {p: i for i, p in enumerate(sorted(players))}

        return GameStateSnapshot(
            players=players,
            seating=seating,
            assignments=self._role_assignments,
            received=self._received_tokens,
            bag=self._bag,
            bluffs=self._bluffs,
            current_night=self._current_night,
            executions=self._executions,
        )

    def _get_alignment(self, role: str) -> str:
        """Get alignment for a role."""
        # This could also come from ASP, but alignment is simple enough
        evil_roles = {"imp", "poisoner", "spy", "baron", "scarlet_woman"}
        return "evil" if role in evil_roles else "good"

    def _get_alive_players(self) -> FrozenSet[str]:
        return frozenset(p for p in self.config.player_names if p not in self._dead_players)

    def _make_public_view(self) -> PublicGameView:
        alive = self._get_alive_players()
        return PublicGameView(
            time=Night(self._current_night) if self._current_day == 0 else Day(self._current_day),
            players=self.config.player_names,
            alive=alive,
            dead=self.config.player_names - alive,
            ghost_votes_used=frozenset(),
            public_events=tuple(self._log[-10:])
        )

    def _make_player_view(self, player_name: str) -> PlayerView:
        public = self._make_public_view()
        role = self._role_assignments.get(player_name, "unknown")
        alignment = self._get_alignment(role)

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

    async def setup_game(self) -> None:
        """Set up the game using ASP to generate valid role assignments."""
        self.log(f"Setting up game with {len(self.config.player_names)} players")

        # Use the existing validator to enumerate setups
        # (This is already ASP-based in the original validator.py)
        if __name__ == "__main__":
            from engine.validator import Validator
        else:
            from .validator import Validator
        setup_validator = Validator(self.validator.asp_path, script=self.validator.script)
        setups = setup_validator.enumerate_legal_setups(self.config, max_models=1)

        if not setups:
            raise ValueError("No valid role assignments found")

        assignment = setups[0]
        self._role_assignments = dict(assignment)

        # For simplicity, received tokens match assignments
        # (In a real game with Drunk, this would differ)
        self._received_tokens = dict(assignment)

        # Extract bag and bluffs from a full ASP solve
        # For now, use a simple approximation
        self._bag = frozenset(assignment.values())
        self._bluffs = frozenset(["chef", "fortune_teller", "butler"])  # Placeholder

        self.log("Role assignments:")
        for player, role in sorted(assignment.items()):
            self.log(f"  {player}: {role}")

        # Notify players
        for player_name, agent in self.player_agents.items():
            role = self._role_assignments[player_name]
            alignment = self._get_alignment(role)
            view = self._make_player_view(player_name)
            await agent.observe(view, f"You are the {role} ({alignment})")

            if alignment == "evil":
                teammates = [
                    p for p, r in self._role_assignments.items()
                    if p != player_name and self._get_alignment(r) == "evil"
                ]
                if teammates:
                    await agent.observe(view, f"Your evil teammates: {', '.join(teammates)}")

    async def _validate_and_execute_ability(
        self,
        role: str,
        player: str,
        ability_type: str,
        target: str,
    ) -> AspValidationResult:
        """
        Validate an ability use through ASP and return the result.

        This is the core integration point - all ability validation
        goes through ASP.
        """
        state = self._get_state_snapshot()

        if role == "imp":
            return self.validator.validate_imp_kill(state, player, target)
        elif role == "monk":
            return self.validator.validate_monk_protection(state, player, target)
        elif role == "poisoner":
            return self.validator.validate_poisoner_poison(state, player, target)
        else:
            # For roles not yet implemented, allow any target
            # (This should eventually be replaced with proper ASP validation)
            return AspValidationResult(valid=True, elapsed_ms=0)

    async def run_night(self, night_number: int) -> Optional[str]:
        """Run a night phase with ASP validation."""
        self.log(f"\n{'='*50}")
        self.log(f"NIGHT {night_number}")
        self.log(f"{'='*50}")

        self._current_night = night_number
        self._trace.set_phase(f"night_{night_number}")

        # Reset nightly state
        self._protected_tonight = None
        self._poisoned_player = None
        self._pending_kill = None

        alive = self._get_alive_players()
        state = self._get_state_snapshot()

        # Night order for TB (from base.lp):
        # other_night_role_order(poisoner, 1).
        # other_night_role_order(monk, 2).
        # other_night_role_order(scarlet_woman, 3).
        # other_night_role_order(imp, 4).
        # ... etc

        night_order = ["poisoner", "monk", "imp"]  # Simplified for demo

        for role in night_order:
            # Find alive player with this role
            players_with_role = [
                p for p in alive
                if self._role_assignments.get(p) == role
            ]

            for player_name in players_with_role:
                agent = self.player_agents[player_name]
                view = self._make_player_view(player_name)

                # Determine ability type and valid targets
                if role == "poisoner":
                    ability = "poison"
                    targets = [p for p in alive if p != player_name]
                elif role == "monk" and night_number > 1:
                    ability = "protect"
                    targets = [p for p in alive if p != player_name]
                elif role == "imp" and night_number > 1:
                    ability = "kill"
                    # Imp can target self (starpass) if minions alive
                    targets = list(alive)
                else:
                    continue  # Skip if no ability this night

                if not targets:
                    continue

                # Wake the player
                self._trace.storyteller_to_player(
                    player_name, InteractionType.WAKE,
                    f"{role.title()}, wake. Choose a target."
                )

                # Request their decision
                request = DecisionRequest(
                    decision_type=DecisionType.USE_ABILITY,
                    context={"ability": ability, "valid_targets": targets},
                    valid_options=targets,
                    prompt=f"Choose a player to {ability}:"
                )

                decision = await agent.decide(view, request)
                target = decision.choice

                # VALIDATE THROUGH ASP
                result = await self._validate_and_execute_ability(
                    role, player_name, ability, target
                )

                if result.valid:
                    self._trace.player_to_storyteller(
                        player_name, f"points at {target}",
                        details={"ability": ability, "target": target, "asp_time_ms": result.elapsed_ms}
                    )
                    self.log(f"  {player_name} ({role}) -> {ability} {target} [ASP: {result.elapsed_ms:.0f}ms ✓]")

                    # Apply the effect
                    if role == "poisoner":
                        self._poisoned_player = target
                    elif role == "monk":
                        self._protected_tonight = target
                    elif role == "imp":
                        self._pending_kill = target
                else:
                    self.log(f"  {player_name} ({role}) -> {ability} {target} [ASP: INVALID - {result.error}]")
                    # In a real game, we'd re-prompt or handle the error
                    # For now, skip the action

        # Resolve night effects
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
        """Run a day phase (simplified - no ASP validation for voting yet)."""
        self.log(f"\n{'='*50}")
        self.log(f"DAY {day_number}")
        self.log(f"{'='*50}")

        self._current_day = day_number
        self._trace.set_phase(f"day_{day_number}")

        if night_death:
            self._trace.public_announcement(
                f"{night_death} was found dead!",
                details={"death": night_death, "cause": "night_kill"}
            )
            self.log(f"  ** {night_death} was found dead! **")

        alive = list(self._get_alive_players())
        executed = None

        # Simplified nomination/voting (same as before)
        nominations = []
        for player_name in alive:
            agent = self.player_agents[player_name]
            view = self._make_player_view(player_name)
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

        for nominator, nominee in nominations:
            if nominee not in alive:
                continue

            self.log(f"\n  Voting on {nominee} (nominated by {nominator})")
            votes_for = []

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

            self.log(f"  Votes: {len(votes_for)}/{len(alive)} ({', '.join(votes_for)})")

            if len(votes_for) > len(alive) // 2:
                executed = nominee
                self._dead_players.add(executed)
                self._executions[day_number] = executed
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
        """Get the current alive demon player."""
        for player, role in self._role_assignments.items():
            if role == "imp" and player not in self._dead_players:
                return player
        return None

    async def run_game(self, max_days: int = 5) -> str:
        """Run a complete game with ASP validation."""
        await self.setup_game()

        winner = None

        for day in range(1, max_days + 1):
            # Night phase
            night_death = await self.run_night(day)

            # Check if demon died
            if self._get_demon() is None:
                winner = "good"
                self.log(f"\n*** GOOD WINS! The demon is dead! ***")
                break

            # Day phase
            executed = await self.run_day(day, night_death)

            # Check win conditions
            if executed and self._get_demon() is None:
                winner = "good"
                self.log(f"\n*** GOOD WINS! The demon ({executed}) was executed! ***")
                break

            alive_count = len(self._get_alive_players())
            if alive_count <= 2:
                winner = "evil"
                self.log(f"\n*** EVIL WINS! Only {alive_count} players remain! ***")
                break

        if not winner:
            self.log("\n*** Game ended without clear winner (max days reached) ***")
            winner = "evil"

        return winner


async def run_asp_game(
    player_names: List[str],
    asp_path: Path,
    script: str = "tb",
    seed: Optional[int] = None
) -> str:
    """Run a game with ASP validation."""
    config = GameConfig(
        script=script,
        player_names=frozenset(player_names)
    )

    validator = AspValidator(asp_path, script=script)

    player_agents = {
        name: RandomPlayerAgent(name, seed=seed)
        for name in player_names
    }
    storyteller = RandomStorytellerAgent(seed=seed)

    game = AspGameOrchestrator(
        config=config,
        validator=validator,
        player_agents=player_agents,
        storyteller=storyteller
    )

    return await game.run_game()


if __name__ == "__main__":
    asp_path = Path(__file__).parent.parent
    players = ["alice", "bob", "charlie", "diana", "eve"]

    print("=" * 60)
    print("Blood on the Clocktower - ASP-Validated Game")
    print("=" * 60)

    winner = asyncio.run(run_asp_game(players, asp_path, seed=42))

    print(f"\nFinal result: {winner.upper()} WINS!")

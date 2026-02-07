"""
Agent abstraction for BotC game engine.

An Agent is anything that can make decisions in the game:
- Human players (via CLI, web UI, etc.)
- AI players (LLM-based)
- Random/scripted agents (for testing)
- The Storyteller (special agent that manages game flow)

Design principles:
- Agents receive only the information they should know
- Agents propose transitions; the validator checks legality
- SCRIPT-AGNOSTIC: Agent interface doesn't know about specific roles
"""

from __future__ import annotations
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Sequence, Optional, FrozenSet, Mapping, Any
from enum import Enum, auto

from .types import (
    GameState, GameTransition, Time,
    UseAbilityTransition, NominateTransition, CastVoteTransition,
    ProvideInfoTransition, SetupBagTransition, AssignRoleTransition
)


# =============================================================================
# Views (filtered information for agents)
# =============================================================================

@dataclass(frozen=True)
class PublicGameView:
    """
    Information visible to all players.

    This is the "public" state of the game that everyone can see.
    """
    time: Time
    players: FrozenSet[str]  # All players (seating order preserved elsewhere)
    alive: FrozenSet[str]
    dead: FrozenSet[str]
    ghost_votes_used: FrozenSet[str]  # Dead players who've used their vote

    # Public events (deaths, executions, nominations, etc.)
    public_events: Sequence[str]  # Human-readable event descriptions


@dataclass(frozen=True)
class PlayerView:
    """
    What a specific player knows about the game.

    Includes public info plus their private info.
    """
    public: PublicGameView

    # My identity
    my_name: str
    my_role: str  # Role name (string, not a type - script-agnostic)
    my_alignment: str  # "good" or "evil"

    # Information I've received from the storyteller
    received_info: Sequence[ReceivedInfo] = field(default_factory=tuple)

    # For evil players: teammates (if known)
    known_teammates: Optional[FrozenSet[str]] = None

    # Special abilities info (e.g., Spy sees grimoire)
    special_knowledge: Optional[Mapping[str, Any]] = None


@dataclass(frozen=True)
class ReceivedInfo:
    """Information received from the storyteller."""
    time: Time
    info_type: str
    content: Mapping[str, Any]
    description: str  # Human-readable


@dataclass(frozen=True)
class StorytellerView:
    """
    The storyteller's view - the full grimoire.

    The storyteller sees everything.
    """
    state: GameState

    # Computed from state (these would be populated by validator)
    role_assignments: Mapping[str, str]  # player -> role
    statuses: Mapping[str, FrozenSet[str]]  # player -> set of statuses


# =============================================================================
# Decision Protocol
# =============================================================================

class DecisionType(Enum):
    """Types of decisions agents can make."""
    # Universal
    USE_ABILITY = auto()        # Use an ability (target selection, etc.)
    NOMINATE = auto()           # Nominate a player
    VOTE = auto()               # Vote yes/no on execution

    # Storyteller only
    SETUP_BAG = auto()          # Choose roles for the bag
    ASSIGN_ROLES = auto()       # Assign roles to players
    PROVIDE_INFO = auto()       # Give info to a player
    RESOLVE_CHOICE = auto()     # Resolve ambiguous game situations


@dataclass(frozen=True)
class DecisionRequest:
    """
    A request for an agent to make a decision.

    The request is abstract - it doesn't encode role-specific logic.
    The context dict provides relevant info for the decision.
    """
    decision_type: DecisionType
    context: Mapping[str, Any]

    # If the valid options are enumerable, they're listed here
    valid_options: Optional[Sequence[Any]] = None

    # Human-readable prompt
    prompt: str = ""


@dataclass(frozen=True)
class Decision:
    """An agent's response to a decision request."""
    decision_type: DecisionType
    choice: Any

    def to_transition(self, player: str) -> Optional[GameTransition]:
        """Convert this decision to a game transition."""
        if self.decision_type == DecisionType.NOMINATE:
            return NominateTransition(nominator=player, nominee=self.choice)
        elif self.decision_type == DecisionType.VOTE:
            return CastVoteTransition(voter=player, vote_yes=self.choice)
        elif self.decision_type == DecisionType.USE_ABILITY:
            # choice should be {"ability": str, "parameters": dict}
            return UseAbilityTransition(
                player=player,
                ability=self.choice["ability"],
                parameters=self.choice.get("parameters", {})
            )
        elif self.decision_type == DecisionType.PROVIDE_INFO:
            return ProvideInfoTransition(
                player=self.choice["player"],
                info_type=self.choice["info_type"],
                content=self.choice["content"]
            )
        return None


# =============================================================================
# Agent Interfaces
# =============================================================================

class PlayerAgent(ABC):
    """
    Interface for player agents.

    A player agent makes decisions from a single player's perspective.
    """

    @property
    @abstractmethod
    def name(self) -> str:
        """The player's name."""
        pass

    @abstractmethod
    async def decide(
        self,
        view: PlayerView,
        request: DecisionRequest
    ) -> Decision:
        """Make a decision based on current view."""
        pass

    @abstractmethod
    async def observe(
        self,
        view: PlayerView,
        event_description: str
    ) -> None:
        """Observe a game event."""
        pass


class StorytellerAgent(ABC):
    """
    Interface for storyteller agents.

    The storyteller manages game flow and provides information.
    """

    @abstractmethod
    async def decide(
        self,
        view: StorytellerView,
        request: DecisionRequest
    ) -> Decision:
        """Make a storyteller decision."""
        pass

    @abstractmethod
    async def on_validation_failure(
        self,
        view: StorytellerView,
        transition: GameTransition,
        error: str
    ) -> GameTransition:
        """Handle a validation failure - propose alternative."""
        pass


# =============================================================================
# Concrete Implementations
# =============================================================================

class RandomPlayerAgent(PlayerAgent):
    """Player that makes random legal choices."""

    def __init__(self, name: str, seed: Optional[int] = None):
        self._name = name
        import random
        self._rng = random.Random(seed)

    @property
    def name(self) -> str:
        return self._name

    async def decide(
        self,
        view: PlayerView,
        request: DecisionRequest
    ) -> Decision:
        if request.valid_options:
            choice = self._rng.choice(list(request.valid_options))
        else:
            choice = self._default_choice(request, view)
        return Decision(request.decision_type, choice)

    def _default_choice(
        self,
        request: DecisionRequest,
        view: PlayerView
    ) -> Any:
        """Make a default random choice for open-ended requests."""
        if request.decision_type == DecisionType.NOMINATE:
            candidates = [p for p in view.public.alive if p != view.my_name]
            return self._rng.choice(candidates) if candidates else None

        elif request.decision_type == DecisionType.VOTE:
            return self._rng.choice([True, False])

        elif request.decision_type == DecisionType.USE_ABILITY:
            # Need context to know what ability and valid targets
            ability = request.context.get("ability", "unknown")
            targets = request.context.get("valid_targets", list(view.public.players))
            target = self._rng.choice(targets) if targets else None
            return {"ability": ability, "parameters": {"target": target}}

        return None

    async def observe(
        self,
        view: PlayerView,
        event_description: str
    ) -> None:
        pass  # Random agent doesn't track history


class RandomStorytellerAgent(StorytellerAgent):
    """Storyteller that makes random legal choices."""

    def __init__(self, seed: Optional[int] = None):
        import random
        self._rng = random.Random(seed)

    async def decide(
        self,
        view: StorytellerView,
        request: DecisionRequest
    ) -> Decision:
        if request.valid_options:
            choice = self._rng.choice(list(request.valid_options))
            return Decision(request.decision_type, choice)

        # For open-ended decisions, we need the validator to enumerate options
        raise NotImplementedError(
            f"Random storyteller needs valid_options for {request.decision_type}"
        )

    async def on_validation_failure(
        self,
        view: StorytellerView,
        transition: GameTransition,
        error: str
    ) -> GameTransition:
        raise ValueError(f"Random storyteller cannot recover: {error}")


class CLIPlayerAgent(PlayerAgent):
    """Human player via command line."""

    def __init__(self, name: str):
        self._name = name

    @property
    def name(self) -> str:
        return self._name

    async def decide(
        self,
        view: PlayerView,
        request: DecisionRequest
    ) -> Decision:
        print(f"\n{'='*50}")
        print(f"{self._name}'s turn")
        print(f"You are: {view.my_role} ({view.my_alignment})")
        print(f"Alive: {', '.join(sorted(view.public.alive))}")
        print(f"{'='*50}")
        print(f"\n{request.prompt}")

        if request.valid_options:
            for i, opt in enumerate(request.valid_options, 1):
                print(f"  {i}. {opt}")
            while True:
                try:
                    idx = int(input("Choose (number): ")) - 1
                    choice = request.valid_options[idx]
                    break
                except (ValueError, IndexError):
                    print("Invalid, try again")
        else:
            choice = input("Your choice: ")

        return Decision(request.decision_type, choice)

    async def observe(
        self,
        view: PlayerView,
        event_description: str
    ) -> None:
        print(f"[{self._name}] {event_description}")

"""
Core type definitions for the BotC game engine.

Design principles:
- Immutable data structures (frozen dataclasses) for game state
- Clear separation between events (things that happened) and state (derived facts)
- Time is explicit and structured (matches ASP model)
- Transitions are first-class objects that can be validated
- SCRIPT-AGNOSTIC: No role-specific logic here; that lives in ASP definitions
"""

from __future__ import annotations
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import FrozenSet, Mapping, Sequence, Optional, Union, Any
from abc import ABC, abstractmethod


# =============================================================================
# Time Model (matches ASP: setup, night(N,R,S), day(N,S))
# =============================================================================

class PhaseType(Enum):
    SETUP = auto()
    NIGHT = auto()
    DAY = auto()


@dataclass(frozen=True)
class Setup:
    """The setup phase before the game begins."""
    phase_type: PhaseType = field(default=PhaseType.SETUP, init=False)

    def to_asp(self) -> str:
        return "setup"


@dataclass(frozen=True)
class Night:
    """A night phase. round/step track sub-phases for role activations."""
    number: int
    round: int = 0
    step: int = 0
    phase_type: PhaseType = field(default=PhaseType.NIGHT, init=False)

    def to_asp(self) -> str:
        return f"night({self.number},{self.round},{self.step})"


@dataclass(frozen=True)
class Day:
    """A day phase. step tracks sub-phases (nominations, votes, etc.)."""
    number: int
    step: int = 0
    phase_type: PhaseType = field(default=PhaseType.DAY, init=False)

    def to_asp(self) -> str:
        return f"day({self.number},{self.step})"


Time = Union[Setup, Night, Day]


# =============================================================================
# Role (abstract - actual roles defined in ASP)
# =============================================================================

@dataclass(frozen=True)
class RoleRef:
    """
    A reference to a role by name.

    The actual role properties (category, alignment, abilities) are defined
    in ASP. This is just a typed string wrapper for clarity.
    """
    name: str

    def to_asp(self) -> str:
        return self.name


# =============================================================================
# Player
# =============================================================================

@dataclass(frozen=True)
class PlayerRef:
    """A reference to a player by name."""
    name: str

    def to_asp(self) -> str:
        return self.name


# =============================================================================
# Game Events (things that happen, cause state changes)
# These are ABSTRACT - they describe what happened, not role-specific mechanics
# =============================================================================

@dataclass(frozen=True)
class Event(ABC):
    """Base class for game events."""
    time: Time

    @abstractmethod
    def to_asp(self) -> str:
        """Convert to ASP fact."""
        pass


@dataclass(frozen=True)
class DeathEvent(Event):
    """A player died."""
    player: str
    cause: str  # Free-form: "execution", "night_kill", "ability", etc.

    def to_asp(self) -> str:
        return f"d_died({self.player},{self.time.to_asp()})"


@dataclass(frozen=True)
class RoleChangeEvent(Event):
    """A player's role changed."""
    player: str
    new_role: str

    def to_asp(self) -> str:
        return f"d_role_change({self.player},{self.new_role},{self.time.to_asp()})"


@dataclass(frozen=True)
class StatusChangeEvent(Event):
    """
    A player gained or lost a status effect.

    Status effects are abstract strings that ASP understands:
    - "poisoned", "drunk", "protected", "mad", etc.
    """
    player: str
    status: str
    gained: bool  # True = gained, False = lost

    def to_asp(self) -> str:
        if self.gained:
            return f"d_gained_status({self.player},{self.status},{self.time.to_asp()})"
        else:
            return f"d_lost_status({self.player},{self.status},{self.time.to_asp()})"


@dataclass(frozen=True)
class InfoGivenEvent(Event):
    """
    Information was given to a player.

    The info_type and content are abstract - ASP defines what's valid.
    """
    player: str
    info_type: str
    content: Mapping[str, Any]

    def to_asp(self) -> str:
        # Convert content to ASP-friendly format
        content_str = ",".join(f"{k}={v}" for k, v in self.content.items())
        return f"d_info_given({self.player},{self.info_type},{self.time.to_asp()})"


@dataclass(frozen=True)
class NominationEvent(Event):
    """A player nominated another player."""
    nominator: str
    nominee: str

    def to_asp(self) -> str:
        return f"d_nominated({self.nominator},{self.nominee},{self.time.to_asp()})"


@dataclass(frozen=True)
class VoteEvent(Event):
    """A vote occurred."""
    nominee: str
    vote_count: int
    threshold_met: bool

    def to_asp(self) -> str:
        return f"d_vote({self.nominee},{self.vote_count},{self.time.to_asp()})"


@dataclass(frozen=True)
class ExecutionEvent(Event):
    """A player was executed."""
    player: str

    def to_asp(self) -> str:
        return f"d_executed({self.player},{self.time.to_asp()})"


@dataclass(frozen=True)
class AbilityUsedEvent(Event):
    """
    An ability was used.

    This is intentionally abstract - the ability name and parameters
    are interpreted by ASP based on the role definitions.
    """
    player: str
    ability: str
    parameters: Mapping[str, Any]  # e.g., {"target": "alice"}

    def to_asp(self) -> str:
        params = ",".join(str(v) for v in self.parameters.values())
        if params:
            return f"d_ability_used({self.player},{self.ability},{params},{self.time.to_asp()})"
        return f"d_ability_used({self.player},{self.ability},{self.time.to_asp()})"


# Union of all event types
GameEvent = Union[
    DeathEvent, RoleChangeEvent, StatusChangeEvent, InfoGivenEvent,
    NominationEvent, VoteEvent, ExecutionEvent, AbilityUsedEvent
]


# =============================================================================
# Game State
# =============================================================================

@dataclass(frozen=True)
class GameConfig:
    """
    Configuration for a game - what script, how many players, etc.
    """
    script: str  # e.g., "trouble_brewing", "sects_and_violets"
    player_names: FrozenSet[str]
    # Additional script-specific config can go in ASP


@dataclass(frozen=True)
class GameState:
    """
    The state of a game, represented as a sequence of events.

    The "current state" (who's alive, who's poisoned, etc.) is DERIVED
    by the ASP solver from these events. We don't duplicate that logic here.
    """
    config: GameConfig
    time: Time
    events: Sequence[GameEvent]

    # Cached queries (populated by validator)
    # These are Optional because they require running ASP to compute
    _alive_players: Optional[FrozenSet[str]] = None
    _winner: Optional[str] = None  # "good", "evil", or None

    def to_asp(self) -> str:
        """Convert entire game state to ASP facts."""
        lines = []

        # Config
        lines.append(f'script({self.config.script}).')
        for player in self.config.player_names:
            lines.append(f'player({player}).')

        # Current time
        lines.append(f'current_time({self.time.to_asp()}).')

        # All events
        for event in self.events:
            lines.append(f'{event.to_asp()}.')

        return "\n".join(lines)

    def is_over(self) -> bool:
        return self._winner is not None


# =============================================================================
# Transitions (proposed changes that need validation)
# These are ABSTRACT - no role-specific transitions
# =============================================================================

@dataclass(frozen=True)
class Transition(ABC):
    """Base class for proposed game transitions."""

    @abstractmethod
    def to_asp(self) -> str:
        """Convert to ASP facts for validation."""
        pass


@dataclass(frozen=True)
class SetupBagTransition(Transition):
    """Propose a bag of roles for the game."""
    roles: FrozenSet[str]

    def to_asp(self) -> str:
        return "\n".join(f"propose_bag({role})." for role in self.roles)


@dataclass(frozen=True)
class AssignRoleTransition(Transition):
    """Propose a role assignment for a player."""
    player: str
    role: str

    def to_asp(self) -> str:
        return f"propose_assignment({self.player},{self.role})."


@dataclass(frozen=True)
class UseAbilityTransition(Transition):
    """
    Propose using an ability.

    The ability name and parameters are abstract - ASP validates
    whether this is legal for the player's role.
    """
    player: str
    ability: str
    parameters: Mapping[str, Any]

    def to_asp(self) -> str:
        params = ",".join(str(v) for v in self.parameters.values())
        if params:
            return f"propose_ability({self.player},{self.ability},{params})."
        return f"propose_ability({self.player},{self.ability})."


@dataclass(frozen=True)
class NominateTransition(Transition):
    """Propose a nomination."""
    nominator: str
    nominee: str

    def to_asp(self) -> str:
        return f"propose_nomination({self.nominator},{self.nominee})."


@dataclass(frozen=True)
class CastVoteTransition(Transition):
    """Propose a vote."""
    voter: str
    vote_yes: bool

    def to_asp(self) -> str:
        if self.vote_yes:
            return f"propose_vote_yes({self.voter})."
        return f"propose_vote_no({self.voter})."


@dataclass(frozen=True)
class ProvideInfoTransition(Transition):
    """
    Storyteller proposes to give information to a player.

    The info_type and content are validated by ASP against what's
    legal for the player's role and game state.
    """
    player: str
    info_type: str
    content: Mapping[str, Any]

    def to_asp(self) -> str:
        # The content needs to be encoded in a way ASP can validate
        content_facts = []
        for k, v in self.content.items():
            content_facts.append(f"propose_info_content({self.player},{k},{v}).")
        return f"propose_info({self.player},{self.info_type}).\n" + "\n".join(content_facts)


@dataclass(frozen=True)
class AdvancePhaseTransition(Transition):
    """Propose advancing to the next phase."""

    def to_asp(self) -> str:
        return "propose_advance_phase."


# Union of all transition types
GameTransition = Union[
    SetupBagTransition, AssignRoleTransition, UseAbilityTransition,
    NominateTransition, CastVoteTransition, ProvideInfoTransition,
    AdvancePhaseTransition
]


# =============================================================================
# Validation Result
# =============================================================================

@dataclass(frozen=True)
class ValidationResult:
    """Result of validating a transition."""
    valid: bool
    new_state: Optional[GameState] = None  # If valid, the resulting state
    error: Optional[str] = None  # If invalid, why

    # For debugging/introspection
    asp_program: Optional[str] = None  # The full ASP program used
    asp_result: Optional[str] = None  # Raw clingo output

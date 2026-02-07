"""
BotC Engine: Script-agnostic game engine for Blood on the Clocktower.

The engine orchestrates gameplay while ASP handles all rule validation.
"""

from .types import (
    # Time
    Time, Setup, Night, Day, PhaseType,
    # References
    RoleRef, PlayerRef,
    # Events
    Event, DeathEvent, RoleChangeEvent, StatusChangeEvent,
    InfoGivenEvent, NominationEvent, VoteEvent, ExecutionEvent,
    AbilityUsedEvent, GameEvent,
    # State
    GameConfig, GameState,
    # Transitions
    Transition, SetupBagTransition, AssignRoleTransition,
    UseAbilityTransition, NominateTransition, CastVoteTransition,
    ProvideInfoTransition, AdvancePhaseTransition, GameTransition,
    # Results
    ValidationResult,
)

from .agent import (
    # Views
    PublicGameView, PlayerView, ReceivedInfo, StorytellerView,
    # Decisions
    DecisionType, DecisionRequest, Decision,
    # Agents
    PlayerAgent, StorytellerAgent,
    RandomPlayerAgent, RandomStorytellerAgent, CLIPlayerAgent,
)

from .validator import Validator, AspResult
from .game import GameOrchestrator, run_simple_game, run_simple_game_sync
from .trace import GameTrace, Interaction, InteractionType
from .asp_validator import AspValidator, GameStateSnapshot, AspValidationResult
from .asp_game import AspGameOrchestrator, run_asp_game

__all__ = [
    # Time
    "Time", "Setup", "Night", "Day", "PhaseType",
    # References
    "RoleRef", "PlayerRef",
    # Events
    "Event", "DeathEvent", "RoleChangeEvent", "StatusChangeEvent",
    "InfoGivenEvent", "NominationEvent", "VoteEvent", "ExecutionEvent",
    "AbilityUsedEvent", "GameEvent",
    # State
    "GameConfig", "GameState",
    # Transitions
    "Transition", "SetupBagTransition", "AssignRoleTransition",
    "UseAbilityTransition", "NominateTransition", "CastVoteTransition",
    "ProvideInfoTransition", "AdvancePhaseTransition", "GameTransition",
    # Results
    "ValidationResult", "AspResult",
    # Views
    "PublicGameView", "PlayerView", "ReceivedInfo", "StorytellerView",
    # Decisions
    "DecisionType", "DecisionRequest", "Decision",
    # Agents
    "PlayerAgent", "StorytellerAgent",
    "RandomPlayerAgent", "RandomStorytellerAgent", "CLIPlayerAgent",
    # Validator
    "Validator",
    # Game
    "GameOrchestrator", "run_simple_game", "run_simple_game_sync",
    # ASP-validated game
    "AspValidator", "GameStateSnapshot", "AspValidationResult",
    "AspGameOrchestrator", "run_asp_game",
    # Trace
    "GameTrace", "Interaction", "InteractionType",
]

"""
Game trace logging and message sequence chart rendering.

Captures interactions between agents during gameplay and can render
them to various visualization formats.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Optional, Literal, Sequence
from enum import Enum, auto


class InteractionType(Enum):
    """Types of interactions between agents."""
    # Storyteller → Player
    WAKE = auto()           # "Wake up" signal
    INFO = auto()           # Information given (Washerwoman sees X, etc.)
    PROMPT = auto()         # Asking for a decision

    # Player → Storyteller
    CHOICE = auto()         # Player's decision (target, vote, etc.)

    # Public (broadcast)
    ANNOUNCE = auto()       # Public announcement (death, execution, phase change)
    NOMINATE = auto()       # Public nomination
    VOTE = auto()           # Vote cast (can be public or private depending on variant)

    # Internal/meta
    PHASE = auto()          # Phase marker (Night 1, Day 2, etc.)


@dataclass(frozen=True)
class Interaction:
    """A single interaction in the game trace."""
    interaction_type: InteractionType
    sender: str             # "storyteller", player name, or "system"
    receiver: str           # player name, "storyteller", "all", or "system"
    message: str            # Human-readable description
    timestamp: int          # Sequence number within the game
    phase: str              # "night_1", "day_2", etc.
    details: Optional[dict] = None  # Structured data for the interaction


@dataclass
class GameTrace:
    """
    A trace of all interactions during a game.

    Can be rendered to various formats for visualization.
    """
    interactions: List[Interaction] = field(default_factory=list)
    _timestamp: int = field(default=0, repr=False)
    _current_phase: str = field(default="setup", repr=False)

    def set_phase(self, phase: str) -> None:
        """Set the current phase for subsequent interactions."""
        self._current_phase = phase
        self.add(
            InteractionType.PHASE,
            "system", "system",
            f"=== {phase.upper().replace('_', ' ')} ===",
            details={"phase": phase}
        )

    def add(
        self,
        interaction_type: InteractionType,
        sender: str,
        receiver: str,
        message: str,
        details: Optional[dict] = None
    ) -> None:
        """Add an interaction to the trace."""
        self.interactions.append(Interaction(
            interaction_type=interaction_type,
            sender=sender,
            receiver=receiver,
            message=message,
            timestamp=self._timestamp,
            phase=self._current_phase,
            details=details
        ))
        self._timestamp += 1

    def storyteller_to_player(
        self,
        player: str,
        interaction_type: InteractionType,
        message: str,
        details: Optional[dict] = None
    ) -> None:
        """Record storyteller sending something to a player."""
        self.add(interaction_type, "storyteller", player, message, details)

    def player_to_storyteller(
        self,
        player: str,
        message: str,
        details: Optional[dict] = None
    ) -> None:
        """Record player responding to storyteller."""
        self.add(InteractionType.CHOICE, player, "storyteller", message, details)

    def public_announcement(
        self,
        message: str,
        details: Optional[dict] = None
    ) -> None:
        """Record a public announcement."""
        self.add(InteractionType.ANNOUNCE, "storyteller", "all", message, details)

    def nomination(
        self,
        nominator: str,
        nominee: str
    ) -> None:
        """Record a nomination."""
        self.add(
            InteractionType.NOMINATE,
            nominator, "all",
            f"{nominator} nominates {nominee}",
            details={"nominator": nominator, "nominee": nominee}
        )

    def vote(
        self,
        voter: str,
        nominee: str,
        voted_yes: bool,
        is_ghost: bool = False
    ) -> None:
        """Record a vote."""
        vote_str = "YES" if voted_yes else "NO"
        ghost_str = " (ghost)" if is_ghost else ""
        self.add(
            InteractionType.VOTE,
            voter, "all",
            f"{voter} votes {vote_str}{ghost_str}",
            details={"voter": voter, "nominee": nominee, "voted_yes": voted_yes, "is_ghost": is_ghost}
        )

    # =========================================================================
    # Rendering methods
    # =========================================================================

    def to_ascii(self, participants: Optional[Sequence[str]] = None) -> str:
        """
        Render trace as ASCII message sequence chart.

        Args:
            participants: List of participant names to include (in order).
                         If None, auto-detects from trace.
        """
        if participants is None:
            participants = self._detect_participants()

        # Column positions for each participant
        col_width = 15
        cols = {p: i * col_width for i, p in enumerate(participants)}

        lines = []

        # Header with participant names
        header = ""
        for p in participants:
            header += p.center(col_width)
        lines.append(header)
        lines.append("│".center(col_width) * len(participants))

        # Draw each interaction
        for interaction in self.interactions:
            if interaction.interaction_type == InteractionType.PHASE:
                # Phase markers span the whole width
                lines.append("")
                lines.append(interaction.message.center(col_width * len(participants)))
                lines.append("│".center(col_width) * len(participants))
                continue

            sender = interaction.sender
            receiver = interaction.receiver

            # Skip if participants not in our list
            if sender not in cols and sender not in ("system", "storyteller"):
                continue
            if receiver not in cols and receiver not in ("all", "system", "storyteller"):
                continue

            # Determine positions
            if sender == "storyteller":
                sender = participants[0] if "storyteller" not in participants else "storyteller"
            if receiver == "all":
                # Broadcast: show as annotation
                line = "│".center(col_width) * len(participants)
                msg = f"[{interaction.message[:30]}]"
                lines.append(line)
                lines.append(msg.center(col_width * len(participants)))
                continue

            # Point-to-point message
            if sender in cols and receiver in cols:
                src_col = cols[sender]
                dst_col = cols[receiver]

                # Build arrow line
                line_chars = list("│".center(col_width) * len(participants))
                if src_col < dst_col:
                    # Arrow goes right
                    arrow_start = src_col + col_width // 2
                    arrow_end = dst_col + col_width // 2
                    for i in range(arrow_start + 1, arrow_end):
                        line_chars[i] = "─"
                    line_chars[arrow_end] = ">"
                elif src_col > dst_col:
                    # Arrow goes left
                    arrow_start = dst_col + col_width // 2
                    arrow_end = src_col + col_width // 2
                    for i in range(arrow_start + 1, arrow_end):
                        line_chars[i] = "─"
                    line_chars[arrow_start] = "<"

                lines.append("".join(line_chars))

                # Message label
                msg = interaction.message[:25]
                mid = (src_col + dst_col) // 2 + col_width // 2
                label_line = " " * (col_width * len(participants))
                label_start = max(0, mid - len(msg) // 2)
                label_line = label_line[:label_start] + msg + label_line[label_start + len(msg):]
                lines.append(label_line)

            lines.append("│".center(col_width) * len(participants))

        return "\n".join(lines)

    def to_mermaid(self, participants: Optional[Sequence[str]] = None) -> str:
        """
        Render trace as Mermaid sequence diagram.

        Can be rendered by Mermaid-compatible tools or the web UI.
        """
        if participants is None:
            participants = self._detect_participants()

        lines = ["sequenceDiagram"]

        # Declare participants
        for p in participants:
            lines.append(f"    participant {p}")

        current_phase = ""

        for interaction in self.interactions:
            if interaction.interaction_type == InteractionType.PHASE:
                # Add a note for phase changes
                if interaction.details:
                    current_phase = interaction.details.get("phase", "")
                lines.append(f"    Note over {','.join(participants)}: {interaction.message}")
                continue

            sender = interaction.sender
            receiver = interaction.receiver

            # Normalize sender/receiver
            if sender == "system":
                continue
            if sender == "storyteller" and "storyteller" not in participants:
                sender = participants[0]  # Use first participant as proxy
            if receiver == "all":
                # Broadcast: show as note
                lines.append(f"    Note over {','.join(participants)}: {interaction.message}")
                continue
            if receiver == "storyteller" and "storyteller" not in participants:
                receiver = participants[0]

            # Skip if not in participants
            if sender not in participants or receiver not in participants:
                continue

            # Message arrow
            msg = interaction.message.replace('"', "'")
            if interaction.interaction_type in (InteractionType.WAKE, InteractionType.PROMPT):
                lines.append(f"    {sender}->>+{receiver}: {msg}")
            elif interaction.interaction_type == InteractionType.CHOICE:
                lines.append(f"    {sender}-->>-{receiver}: {msg}")
            elif interaction.interaction_type == InteractionType.INFO:
                lines.append(f"    {sender}->>+{receiver}: {msg}")
                lines.append(f"    {receiver}-->>-{sender}: (acknowledges)")
            else:
                lines.append(f"    {sender}->>{receiver}: {msg}")

        return "\n".join(lines)

    def to_plantuml(self, participants: Optional[Sequence[str]] = None) -> str:
        """Render trace as PlantUML sequence diagram."""
        if participants is None:
            participants = self._detect_participants()

        lines = ["@startuml"]

        # Declare participants
        for p in participants:
            lines.append(f'participant "{p}" as {p.replace(" ", "_")}')

        for interaction in self.interactions:
            if interaction.interaction_type == InteractionType.PHASE:
                lines.append(f"== {interaction.message} ==")
                continue

            sender = interaction.sender.replace(" ", "_")
            receiver = interaction.receiver.replace(" ", "_")

            if sender == "system" or receiver == "system":
                continue
            if receiver == "all":
                lines.append(f'note across: {interaction.message}')
                continue

            # Skip if not in participants
            sender_clean = sender.replace("_", " ")
            receiver_clean = receiver.replace("_", " ")
            if sender_clean not in participants and sender != "storyteller":
                continue
            if receiver_clean not in participants and receiver not in ("all", "storyteller"):
                continue

            msg = interaction.message.replace('"', "'")
            lines.append(f'{sender} -> {receiver}: {msg}')

        lines.append("@enduml")
        return "\n".join(lines)

    def _detect_participants(self) -> List[str]:
        """Auto-detect participants from the trace."""
        participants = set()
        for interaction in self.interactions:
            if interaction.sender not in ("system", "all"):
                participants.add(interaction.sender)
            if interaction.receiver not in ("system", "all"):
                participants.add(interaction.receiver)

        # Sort with storyteller first, then alphabetically
        def sort_key(p):
            if p == "storyteller":
                return (0, p)
            return (1, p)

        return sorted(participants, key=sort_key)

    def summary(self) -> str:
        """Return a brief summary of the trace."""
        phases = [i for i in self.interactions if i.interaction_type == InteractionType.PHASE]
        choices = [i for i in self.interactions if i.interaction_type == InteractionType.CHOICE]
        announcements = [i for i in self.interactions if i.interaction_type == InteractionType.ANNOUNCE]

        return (
            f"GameTrace: {len(self.interactions)} interactions, "
            f"{len(phases)} phases, {len(choices)} player choices, "
            f"{len(announcements)} announcements"
        )

#!/usr/bin/env python3
"""
Demo: Run a simple Blood on the Clocktower game with random agents.

This demonstrates the game orchestrator coordinating multiple agents
through a complete game of BotC, then renders a message sequence chart.
"""

import sys
import asyncio
from pathlib import Path

# Add parent to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent))

from engine import (
    GameOrchestrator, GameConfig, Validator,
    RandomPlayerAgent, RandomStorytellerAgent
)


async def run_game_with_trace(players, asp_path, script="tb", seed=42):
    """Run a game and return the orchestrator (with trace access)."""
    config = GameConfig(
        script=script,
        player_names=frozenset(players)
    )

    validator = Validator(asp_path, script=script)

    player_agents = {
        name: RandomPlayerAgent(name, seed=seed)
        for name in players
    }
    storyteller = RandomStorytellerAgent(seed=seed)

    game = GameOrchestrator(
        config=config,
        validator=validator,
        player_agents=player_agents,
        storyteller=storyteller
    )

    winner = await game.run_game()
    return game, winner


def main():
    # Path to ASP files
    asp_path = Path(__file__).parent.parent

    # 5 players for a quick game
    players = ["alice", "bob", "charlie", "diana", "eve"]

    print("=" * 60)
    print("Blood on the Clocktower - Random Agent Demo")
    print("=" * 60)
    print(f"Players: {', '.join(players)}")
    print(f"Script: Trouble Brewing")
    print("=" * 60)

    # Run the game
    game, winner = asyncio.run(
        run_game_with_trace(players, asp_path, script="tb", seed=42)
    )

    print("\n" + "=" * 60)
    print(f"FINAL RESULT: {winner.upper()} WINS!")
    print("=" * 60)

    # Show trace summary
    print(f"\n{game.trace.summary()}")

    # Render as Mermaid (for web viewing)
    print("\n" + "=" * 60)
    print("MERMAID SEQUENCE DIAGRAM")
    print("=" * 60)
    print(game.trace.to_mermaid(["storyteller"] + players))

    # Also save to file
    mermaid_path = asp_path / "engine" / "last_game.mmd"
    mermaid_path.write_text(game.trace.to_mermaid(["storyteller"] + players))
    print(f"\nSaved to: {mermaid_path}")


if __name__ == "__main__":
    main()

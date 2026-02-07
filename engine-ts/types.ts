/**
 * Core type definitions for the BotC TypeScript engine.
 *
 * Mirrors the Python types.py for consistency.
 */

/**
 * Validation mode - full trace vs incremental.
 */
export enum ValidationMode {
  FULL_TRACE = 'full-trace',
  INCREMENTAL = 'incremental',
}

/**
 * Result of an ASP validation query.
 */
export interface AspValidationResult {
  valid: boolean;
  elapsedMs: number;
  error?: string;
  details?: {
    validatedAtoms?: string[];
    mode?: string;
  };
}

/**
 * A snapshot of game state at a specific time for ASP validation.
 *
 * This captures everything ASP needs to validate a single transition.
 */
export interface GameStateSnapshot {
  /** Player names */
  players: Set<string>;

  /** Player -> chair number mapping */
  seating: Map<string, number>;

  /** Player -> role mapping (current assignments) */
  assignments: Map<string, string>;

  /** Player -> token received (usually matches assignments) */
  received: Map<string, string>;

  /** Roles in the bag */
  bag: Set<string>;

  /** 3 bluff roles shown to demon */
  bluffs: Set<string>;

  /** Current night number */
  currentNight: number;

  /** Day -> player executed mapping */
  executions: Map<number, string>;

  /** Additional ASP facts to assert */
  extraFacts?: string;
}

/**
 * Helper to create a GameStateSnapshot from plain objects.
 */
export function createGameState(config: {
  players: string[];
  seating: Record<string, number>;
  assignments: Record<string, string>;
  received?: Record<string, string>;
  bag?: string[];
  bluffs?: string[];
  currentNight: number;
  executions?: Record<number, string>;
  extraFacts?: string;
}): GameStateSnapshot {
  return {
    players: new Set(config.players),
    seating: new Map(Object.entries(config.seating)),
    assignments: new Map(Object.entries(config.assignments)),
    received: new Map(Object.entries(config.received ?? config.assignments)),
    bag: new Set(config.bag ?? Object.values(config.assignments)),
    bluffs: new Set(config.bluffs ?? []),
    currentNight: config.currentNight,
    executions: new Map(
      Object.entries(config.executions ?? {}).map(([k, v]) => [parseInt(k), v])
    ),
    extraFacts: config.extraFacts,
  };
}

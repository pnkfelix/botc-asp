/**
 * ASP-based validator for Blood on the Clocktower.
 *
 * This module provides single-transition validation using the real ASP
 * infrastructure (botc.lp + script files), ensuring all rule validation
 * happens in ASP rather than duplicated TypeScript logic.
 *
 * Two modes are supported:
 * - FULL_TRACE: Uses botc.lp, derives state via inertia from initial conditions.
 *               Slower but validates full game history consistency.
 * - INCREMENTAL: Uses incremental.lp, accepts state as input facts.
 *                Much faster (40-60x), validates single action in isolation.
 */

import * as clingo from 'clingo-wasm';
import * as fs from 'fs';
import * as path from 'path';
import {
  ValidationMode,
  AspValidationResult,
  GameStateSnapshot,
} from './types';

// TB minion roles for starpass check
const MINION_ROLES = new Set(['poisoner', 'spy', 'scarlet_woman', 'baron']);

/**
 * Recursively resolve #include directives.
 * clingo-wasm doesn't support #include, so we inline them.
 */
function resolveIncludes(content: string, basePath: string): string {
  const includeRegex = /#include\s+"([^"]+)"\s*\./g;
  let result = content;
  let match;

  // Keep resolving until no more includes
  while ((match = includeRegex.exec(result)) !== null) {
    const includePath = match[1];
    const fullPath = path.resolve(basePath, includePath);
    const includeContent = fs.readFileSync(fullPath, 'utf-8');

    // Recursively resolve includes in the included file
    const resolvedInclude = resolveIncludes(includeContent, path.dirname(fullPath));

    result = result.replace(match[0], resolvedInclude);
    // Reset regex since we modified the string
    includeRegex.lastIndex = 0;
  }

  return result;
}

/**
 * ASP Validator for BotC transitions.
 *
 * Loads ASP files once, then validates individual transitions by asserting
 * state + proposed action and checking SAT.
 */
export class AspValidator {
  private aspPath: string;
  private script: string;
  private mode: ValidationMode;

  // Cached resolved ASP content
  private baseProgram: string | null = null;
  private scriptProgram: string | null = null;

  constructor(
    aspPath: string,
    script: string = 'tb',
    mode: ValidationMode = ValidationMode.INCREMENTAL
  ) {
    this.aspPath = aspPath;
    this.script = script;
    this.mode = mode;
  }

  /**
   * Load and cache ASP files.
   */
  private ensureLoaded(): void {
    if (this.baseProgram !== null) {
      return;
    }

    // Load the base file based on mode
    const baseFile =
      this.mode === ValidationMode.INCREMENTAL
        ? path.join(this.aspPath, 'incremental.lp')
        : path.join(this.aspPath, 'botc.lp');

    if (!fs.existsSync(baseFile)) {
      throw new Error(`Missing base file at ${baseFile}`);
    }

    const scriptFile = path.join(this.aspPath, `${this.script}.lp`);
    if (!fs.existsSync(scriptFile)) {
      throw new Error(`Missing script ${this.script}.lp at ${scriptFile}`);
    }

    // Load and resolve includes
    const baseContent = fs.readFileSync(baseFile, 'utf-8');
    const scriptContent = fs.readFileSync(scriptFile, 'utf-8');

    this.baseProgram = resolveIncludes(baseContent, this.aspPath);
    this.scriptProgram = resolveIncludes(scriptContent, this.aspPath);
  }

  /**
   * Convert state to ASP facts for incremental mode.
   * Uses inc_* predicates to provide current state directly.
   */
  private stateToAspIncremental(state: GameStateSnapshot): string {
    const lines: string[] = [];

    // Player count
    lines.push(`#const player_count = ${state.players.size}.`);
    lines.push('');

    // Players and seating
    const sortedPlayers = Array.from(state.players).sort();
    for (const player of sortedPlayers) {
      const chair = state.seating.get(player)!;
      lines.push(`name(${player}). chair(${player}, ${chair}).`);
    }
    lines.push('');

    // Current night
    lines.push(`current_night(${state.currentNight}).`);
    lines.push('');

    // Current role assignments (inc_role)
    const sortedAssignments = Array.from(state.assignments.entries()).sort();
    for (const [player, role] of sortedAssignments) {
      lines.push(`inc_role(${player}, ${role}).`);
    }
    lines.push('');

    // Who is alive (anyone not executed)
    const executedPlayers = new Set(state.executions.values());
    for (const player of sortedPlayers) {
      if (!executedPlayers.has(player)) {
        lines.push(`inc_alive(${player}).`);
      }
    }
    lines.push('');

    // Check if any minion is alive
    for (const [player, role] of state.assignments) {
      if (MINION_ROLES.has(role) && !executedPlayers.has(player)) {
        lines.push('inc_minion_alive.');
        break;
      }
    }

    if (state.extraFacts) {
      lines.push('');
      lines.push(state.extraFacts);
    }

    return lines.join('\n');
  }

  /**
   * Convert state to ASP facts for full-trace mode.
   * Uses assigned(0, ...) constraints and derives state via inertia.
   */
  private stateToAspFullTrace(state: GameStateSnapshot): string {
    const lines: string[] = [];

    // Player count
    lines.push(`#const player_count = ${state.players.size}.`);
    lines.push('');

    // Players and seating
    const sortedPlayers = Array.from(state.players).sort();
    for (const player of sortedPlayers) {
      const chair = state.seating.get(player)!;
      lines.push(`name(${player}). chair(${player}, ${chair}).`);
    }
    lines.push('');

    // Request modeling of the current night
    if (state.currentNight > 1) {
      lines.push(`needs_night(${state.currentNight}).`);
      lines.push('');
    }

    // Role assignments (constrain rather than assert)
    const sortedAssignments = Array.from(state.assignments.entries()).sort();
    for (const [player, role] of sortedAssignments) {
      lines.push(`:- not assigned(0, ${player}, ${role}).`);
    }
    lines.push('');

    // Executions (facts)
    const sortedExecutions = Array.from(state.executions.entries()).sort(
      (a, b) => a[0] - b[0]
    );
    for (const [day, player] of sortedExecutions) {
      lines.push(`executed(${player}, ${day}).`);
    }

    if (state.extraFacts) {
      lines.push('');
      lines.push(state.extraFacts);
    }

    return lines.join('\n');
  }

  /**
   * Convert game state to ASP facts based on current mode.
   */
  private stateToAsp(state: GameStateSnapshot): string {
    if (this.mode === ValidationMode.INCREMENTAL) {
      return this.stateToAspIncremental(state);
    } else {
      return this.stateToAspFullTrace(state);
    }
  }

  /**
   * Run a validation query and return the result.
   */
  private async runValidation(program: string): Promise<AspValidationResult> {
    const start = performance.now();

    try {
      this.ensureLoaded();

      const fullProgram = `${this.baseProgram}\n${this.scriptProgram}\n${program}`;
      const result = await clingo.run(fullProgram, 1);

      const elapsed = performance.now() - start;

      if (result.Result === 'SATISFIABLE') {
        const atoms = result.Call?.[0]?.Witnesses?.[0]?.Value || [];
        return {
          valid: true,
          elapsedMs: elapsed,
          details: { validatedAtoms: atoms, mode: this.mode },
        };
      } else {
        return {
          valid: false,
          elapsedMs: elapsed,
          error: 'UNSAT - action violates game constraints',
        };
      }
    } catch (e) {
      const elapsed = performance.now() - start;
      return {
        valid: false,
        elapsedMs: elapsed,
        error: String(e),
      };
    }
  }

  /**
   * Validate an Imp kill action.
   */
  async validateImpKill(
    state: GameStateSnapshot,
    impPlayer: string,
    target: string
  ): Promise<AspValidationResult> {
    // From base.lp: other_night_role_order(imp, 4)
    const impOrder = 4;
    const timePoint = `night(${state.currentNight}, ${impOrder}, 2)`;

    const actionConstraint = `:- not player_chooses(imp, ${impPlayer}, point(${target}), ${timePoint}).`;

    const stateAsp = this.stateToAsp(state);
    const program = `
${stateAsp}

% === PROPOSED ACTION ===
${actionConstraint}

#show player_chooses/4.
`;

    return this.runValidation(program);
  }

  /**
   * Validate a Monk protection action.
   */
  async validateMonkProtection(
    state: GameStateSnapshot,
    monkPlayer: string,
    target: string
  ): Promise<AspValidationResult> {
    // From base.lp: other_night_role_order(monk, 2)
    const monkOrder = 2;
    const timePoint = `night(${state.currentNight}, ${monkOrder}, 2)`;

    const actionConstraint = `:- not player_chooses(monk, ${monkPlayer}, point(${target}), ${timePoint}).`;

    const stateAsp = this.stateToAsp(state);
    const program = `
${stateAsp}

${actionConstraint}

#show player_chooses/4.
`;

    return this.runValidation(program);
  }

  /**
   * Validate a Poisoner poisoning action.
   */
  async validatePoisonerPoison(
    state: GameStateSnapshot,
    poisonerPlayer: string,
    target: string
  ): Promise<AspValidationResult> {
    // From base.lp: other_night_role_order(poisoner, 1)
    const poisonerOrder = 1;
    const timePoint = `night(${state.currentNight}, ${poisonerOrder}, 2)`;

    const actionConstraint = `:- not player_chooses(poisoner, ${poisonerPlayer}, point(${target}), ${timePoint}).`;

    const stateAsp = this.stateToAsp(state);
    const program = `
${stateAsp}

${actionConstraint}

#show player_chooses/4.
`;

    return this.runValidation(program);
  }

  /**
   * Validate a Fortune Teller choice action.
   */
  async validateFortuneTellerChoice(
    state: GameStateSnapshot,
    ftPlayer: string,
    target1: string,
    target2: string
  ): Promise<AspValidationResult> {
    // From base.lp: other_night_role_order(fortune_teller, 7)
    const ftOrder = 7;
    const timePoint = `night(${state.currentNight}, ${ftOrder}, 2)`;

    const actionConstraint = `
:- not player_chooses(fortune_teller, ${ftPlayer}, point(${target1}), ${timePoint}).
:- not player_chooses(fortune_teller, ${ftPlayer}, point(${target2}), ${timePoint}).
`;

    const stateAsp = this.stateToAsp(state);
    const program = `
${stateAsp}

${actionConstraint}

#show player_chooses/4.
`;

    return this.runValidation(program);
  }

  /**
   * Get the current validation mode.
   */
  getMode(): ValidationMode {
    return this.mode;
  }

  /**
   * Switch validation mode.
   */
  setMode(mode: ValidationMode): void {
    if (mode !== this.mode) {
      this.mode = mode;
      // Clear cache to reload correct base file
      this.baseProgram = null;
      this.scriptProgram = null;
    }
  }
}

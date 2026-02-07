/**
 * TypeScript proof-of-concept for BotC ASP validation.
 *
 * This demonstrates that the same ASP validation approach works
 * with clingo-wasm in the browser/Node.js environment.
 */

import * as clingo from 'clingo-wasm';
import * as fs from 'fs';
import * as path from 'path';

interface ValidationResult {
  valid: boolean;
  elapsedMs: number;
  atoms?: string[];
  error?: string;
}

function loadAspFile(filePath: string): string {
  return fs.readFileSync(filePath, 'utf-8');
}

function resolveIncludes(content: string, basePath: string): string {
  /**
   * Recursively resolve #include directives.
   * clingo-wasm doesn't support #include, so we inline them.
   */
  const includeRegex = /#include\s+"([^"]+)"\s*\./g;
  let result = content;
  let match;

  // Keep resolving until no more includes
  while ((match = includeRegex.exec(result)) !== null) {
    const includePath = match[1];
    const fullPath = path.resolve(basePath, includePath);
    const includeContent = loadAspFile(fullPath);

    // Recursively resolve includes in the included file
    const resolvedInclude = resolveIncludes(
      includeContent,
      path.dirname(fullPath)
    );

    result = result.replace(match[0], resolvedInclude);
    // Reset regex since we modified the string
    includeRegex.lastIndex = 0;
  }

  return result;
}

// Cache resolved ASP files to avoid re-processing
let cachedBotc: string | null = null;
let cachedTb: string | null = null;

async function getResolvedAsp(aspPath: string): Promise<{ botc: string; tb: string }> {
  if (!cachedBotc || !cachedTb) {
    const botcLp = loadAspFile(path.join(aspPath, 'botc.lp'));
    const tbLp = loadAspFile(path.join(aspPath, 'tb.lp'));
    cachedBotc = resolveIncludes(botcLp, aspPath);
    cachedTb = resolveIncludes(tbLp, aspPath);
  }
  return { botc: cachedBotc, tb: cachedTb };
}

async function validateImpKill5Players(
  aspPath: string,
  impPlayer: string,
  target: string,
  night: number
): Promise<ValidationResult> {
  const start = performance.now();

  try {
    const { botc, tb } = await getResolvedAsp(aspPath);

    // State for 5-player game
    const stateProgram = `
#const player_count = 5.

name(alice). chair(alice, 0).
name(bob). chair(bob, 1).
name(charlie). chair(charlie, 2).
name(diana). chair(diana, 3).
name(eve). chair(eve, 4).

needs_night(${night}).

:- not assigned(0, alice, washerwoman).
:- not assigned(0, bob, empath).
:- not assigned(0, charlie, monk).
:- not assigned(0, ${impPlayer}, imp).
:- not assigned(0, eve, poisoner).

% Proposed action: ${impPlayer} kills ${target}
:- not player_chooses(imp, ${impPlayer}, point(${target}), night(${night}, 4, 2)).

#show player_chooses/4.
`;

    const fullProgram = `${botc}\n${tb}\n${stateProgram}`;
    const result = await clingo.run(fullProgram, 1);
    const elapsed = performance.now() - start;

    if (result.Result === 'SATISFIABLE') {
      const atoms = result.Call?.[0]?.Witnesses?.[0]?.Value || [];
      return { valid: true, elapsedMs: elapsed, atoms };
    } else {
      return { valid: false, elapsedMs: elapsed, error: 'UNSAT - action violates game constraints' };
    }
  } catch (e) {
    return { valid: false, elapsedMs: performance.now() - start, error: String(e) };
  }
}

async function validateNRB9Players(
  aspPath: string,
  impPlayer: string,
  target: string,
  night: number,
  executedBefore: Record<number, string> = {}
): Promise<ValidationResult> {
  /**
   * Validate using NRB Episode 001 game state (9 players).
   */
  const start = performance.now();

  try {
    const { botc, tb } = await getResolvedAsp(aspPath);

    // Build execution facts
    const executionFacts = Object.entries(executedBefore)
      .map(([day, player]) => `executed(${player}, ${day}).`)
      .join('\n');

    // State for 9-player NRB game
    const stateProgram = `
#const player_count = 9.

% NRB Episode 001 seating
name(luke). chair(luke, 0).
name(oli). chair(oli, 1).
name(blair). chair(blair, 2).
name(tom). chair(tom, 3).
name(elliott). chair(elliott, 4).
name(laurie). chair(laurie, 5).
name(isaac). chair(isaac, 6).
name(jon). chair(jon, 7).
name(sullivan). chair(sullivan, 8).

needs_night(${night}).

% NRB role assignments
:- not assigned(0, luke, imp).
:- not assigned(0, oli, ravenkeeper).
:- not assigned(0, blair, fortune_teller).
:- not assigned(0, tom, monk).
:- not assigned(0, elliott, recluse).
:- not assigned(0, laurie, scarlet_woman).
:- not assigned(0, isaac, undertaker).
:- not assigned(0, jon, chef).
:- not assigned(0, sullivan, drunk).

${executionFacts}

% Proposed action: ${impPlayer} kills ${target}
:- not player_chooses(imp, ${impPlayer}, point(${target}), night(${night}, 4, 2)).

#show player_chooses/4.
`;

    const fullProgram = `${botc}\n${tb}\n${stateProgram}`;
    const result = await clingo.run(fullProgram, 1);
    const elapsed = performance.now() - start;

    if (result.Result === 'SATISFIABLE') {
      const atoms = result.Call?.[0]?.Witnesses?.[0]?.Value || [];
      return { valid: true, elapsedMs: elapsed, atoms };
    } else {
      return { valid: false, elapsedMs: elapsed, error: 'UNSAT - action violates game constraints' };
    }
  } catch (e) {
    return { valid: false, elapsedMs: performance.now() - start, error: String(e) };
  }
}

async function main() {
  console.log('='.repeat(60));
  console.log('TypeScript/clingo-wasm Validation Proof-of-Concept');
  console.log('='.repeat(60));
  console.log();

  // __dirname is dist/ after compilation, so go up two levels to botc-asp/
  const aspPath = path.resolve(__dirname, '..', '..');

  // === 5-Player Tests ===
  console.log('--- 5-Player Tests ---');
  console.log();

  // Test 1: Valid Imp kill (5 players)
  console.log('Test 1: diana (Imp) kills alice [5 players, Night 2]');
  const result1 = await validateImpKill5Players(aspPath, 'diana', 'alice', 2);
  console.log(`  Valid: ${result1.valid}`);
  console.log(`  Time: ${result1.elapsedMs.toFixed(0)}ms`);
  if (result1.error) console.log(`  Error: ${result1.error}`);
  console.log();

  // Test 2: Valid starpass (5 players)
  console.log('Test 2: diana (Imp) starpass [5 players, Night 2]');
  const result2 = await validateImpKill5Players(aspPath, 'diana', 'diana', 2);
  console.log(`  Valid: ${result2.valid}`);
  console.log(`  Time: ${result2.elapsedMs.toFixed(0)}ms`);
  if (result2.error) console.log(`  Error: ${result2.error}`);
  console.log();

  // === 9-Player NRB Tests ===
  console.log('--- 9-Player NRB Tests (Episode 001) ---');
  console.log();

  // Test 3: NRB Night 2 - luke kills sullivan
  console.log('Test 3: luke (Imp) kills sullivan [9 players, Night 2]');
  const result3 = await validateNRB9Players(aspPath, 'luke', 'sullivan', 2);
  console.log(`  Valid: ${result3.valid}`);
  console.log(`  Time: ${result3.elapsedMs.toFixed(0)}ms`);
  if (result3.error) console.log(`  Error: ${result3.error}`);
  console.log();

  // Test 4: NRB Night 3 - luke kills tom (after jon executed day 2)
  console.log('Test 4: luke (Imp) kills tom [9 players, Night 3, after Day 2 execution]');
  const result4 = await validateNRB9Players(aspPath, 'luke', 'tom', 3, { 2: 'jon' });
  console.log(`  Valid: ${result4.valid}`);
  console.log(`  Time: ${result4.elapsedMs.toFixed(0)}ms`);
  if (result4.error) console.log(`  Error: ${result4.error}`);
  console.log();

  // Test 5: NRB Night 4 - luke kills oli (after jon day 2, elliott day 3)
  console.log('Test 5: luke (Imp) kills oli [9 players, Night 4, after Day 2+3 executions]');
  const result5 = await validateNRB9Players(aspPath, 'luke', 'oli', 4, { 2: 'jon', 3: 'elliott' });
  console.log(`  Valid: ${result5.valid}`);
  console.log(`  Time: ${result5.elapsedMs.toFixed(0)}ms`);
  if (result5.error) console.log(`  Error: ${result5.error}`);
  console.log();

  // Summary
  console.log('='.repeat(60));
  console.log('Summary');
  console.log('='.repeat(60));
  console.log();
  console.log('5-Player Results:');
  console.log(`  Test 1 (Imp kill):  ${result1.valid ? 'PASS' : 'FAIL'} - ${result1.elapsedMs.toFixed(0)}ms`);
  console.log(`  Test 2 (Starpass):  ${result2.valid ? 'PASS' : 'FAIL'} - ${result2.elapsedMs.toFixed(0)}ms`);
  console.log();
  console.log('9-Player NRB Results:');
  console.log(`  Test 3 (Night 2):   ${result3.valid ? 'PASS' : 'FAIL'} - ${result3.elapsedMs.toFixed(0)}ms`);
  console.log(`  Test 4 (Night 3):   ${result4.valid ? 'PASS' : 'FAIL'} - ${result4.elapsedMs.toFixed(0)}ms`);
  console.log(`  Test 5 (Night 4):   ${result5.valid ? 'PASS' : 'FAIL'} - ${result5.elapsedMs.toFixed(0)}ms`);
  console.log();

  const allPass = result1.valid && result2.valid && result3.valid && result4.valid && result5.valid;
  if (allPass) {
    console.log('TypeScript portability: CONFIRMED');
    console.log('The same ASP validation approach works in Node.js/browser.');
  } else {
    console.log('TypeScript portability: PARTIAL');
    console.log('Some tests failed - check error messages above.');
  }
}

main().catch(console.error);

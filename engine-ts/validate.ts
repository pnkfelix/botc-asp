/**
 * TypeScript BotC ASP validation - with both full-trace and incremental modes.
 *
 * This demonstrates feature parity with the Python implementation:
 * - Full-trace mode: O(n) with game length, validates complete history
 * - Incremental mode: O(1) ~30-40ms, validates single transition
 */

import * as path from 'path';
import {
  ValidationMode,
  AspValidationResult,
  createGameState,
  GameStateSnapshot,
} from './types';
import { AspValidator } from './asp-validator';

interface TestResult {
  name: string;
  expected: boolean;
  result: AspValidationResult;
  passed: boolean;
}

/**
 * Run tests for both validation modes and compare.
 */
async function runComparisonTests(): Promise<void> {
  console.log('='.repeat(70));
  console.log('TypeScript ASP Validator - Incremental vs Full-Trace Comparison');
  console.log('='.repeat(70));
  console.log();

  // __dirname is dist/ after compilation, so go up two levels to botc-asp/
  const aspPath = path.resolve(__dirname, '..', '..');

  // === Test States ===

  // 5-player game state
  const state5Player = createGameState({
    players: ['alice', 'bob', 'charlie', 'diana', 'eve'],
    seating: { alice: 0, bob: 1, charlie: 2, diana: 3, eve: 4 },
    assignments: {
      alice: 'washerwoman',
      bob: 'empath',
      charlie: 'monk',
      diana: 'imp',
      eve: 'poisoner',
    },
    currentNight: 2,
    executions: {},
  });

  // 5-player with no minion alive (eve executed)
  const state5PlayerNoMinion = createGameState({
    players: ['alice', 'bob', 'charlie', 'diana', 'eve'],
    seating: { alice: 0, bob: 1, charlie: 2, diana: 3, eve: 4 },
    assignments: {
      alice: 'washerwoman',
      bob: 'empath',
      charlie: 'monk',
      diana: 'imp',
      eve: 'poisoner',
    },
    currentNight: 2,
    executions: { 1: 'eve' },
  });

  // 9-player NRB game state
  const stateNRB = createGameState({
    players: [
      'luke', 'oli', 'blair', 'tom', 'elliott',
      'laurie', 'isaac', 'jon', 'sullivan',
    ],
    seating: {
      luke: 0, oli: 1, blair: 2, tom: 3, elliott: 4,
      laurie: 5, isaac: 6, jon: 7, sullivan: 8,
    },
    assignments: {
      luke: 'imp',
      oli: 'ravenkeeper',
      blair: 'fortune_teller',
      tom: 'monk',
      elliott: 'recluse',
      laurie: 'scarlet_woman',
      isaac: 'undertaker',
      jon: 'chef',
      sullivan: 'drunk',
    },
    currentNight: 2,
    executions: {},
  });

  // NRB Night 3 (after day 2 execution)
  const stateNRBNight3 = createGameState({
    players: [
      'luke', 'oli', 'blair', 'tom', 'elliott',
      'laurie', 'isaac', 'jon', 'sullivan',
    ],
    seating: {
      luke: 0, oli: 1, blair: 2, tom: 3, elliott: 4,
      laurie: 5, isaac: 6, jon: 7, sullivan: 8,
    },
    assignments: {
      luke: 'imp',
      oli: 'ravenkeeper',
      blair: 'fortune_teller',
      tom: 'monk',
      elliott: 'recluse',
      laurie: 'scarlet_woman',
      isaac: 'undertaker',
      jon: 'chef',
      sullivan: 'drunk',
    },
    currentNight: 3,
    executions: { 2: 'jon' },
  });

  // NRB Night 4 (after day 2+3 executions)
  const stateNRBNight4 = createGameState({
    players: [
      'luke', 'oli', 'blair', 'tom', 'elliott',
      'laurie', 'isaac', 'jon', 'sullivan',
    ],
    seating: {
      luke: 0, oli: 1, blair: 2, tom: 3, elliott: 4,
      laurie: 5, isaac: 6, jon: 7, sullivan: 8,
    },
    assignments: {
      luke: 'imp',
      oli: 'ravenkeeper',
      blair: 'fortune_teller',
      tom: 'monk',
      elliott: 'recluse',
      laurie: 'scarlet_woman',
      isaac: 'undertaker',
      jon: 'chef',
      sullivan: 'drunk',
    },
    currentNight: 4,
    executions: { 2: 'jon', 3: 'elliott' },
  });

  // === Test Definitions ===
  interface TestCase {
    name: string;
    state: GameStateSnapshot;
    validate: (v: AspValidator) => Promise<AspValidationResult>;
    expected: boolean;
  }

  const testCases: TestCase[] = [
    {
      name: 'Night 2: diana kills alice (valid)',
      state: state5Player,
      validate: (v) => v.validateImpKill(state5Player, 'diana', 'alice'),
      expected: true,
    },
    {
      name: 'Night 2: diana starpass (minion alive)',
      state: state5Player,
      validate: (v) => v.validateImpKill(state5Player, 'diana', 'diana'),
      expected: true,
    },
    {
      name: 'Night 2: diana starpass (NO minion - invalid)',
      state: state5PlayerNoMinion,
      validate: (v) =>
        v.validateImpKill(state5PlayerNoMinion, 'diana', 'diana'),
      expected: false,
    },
    {
      name: 'NRB Night 2: luke kills sullivan',
      state: stateNRB,
      validate: (v) => v.validateImpKill(stateNRB, 'luke', 'sullivan'),
      expected: true,
    },
    {
      name: 'NRB Night 3: luke kills tom',
      state: stateNRBNight3,
      validate: (v) => v.validateImpKill(stateNRBNight3, 'luke', 'tom'),
      expected: true,
    },
    {
      name: 'NRB Night 4: luke kills oli',
      state: stateNRBNight4,
      validate: (v) => v.validateImpKill(stateNRBNight4, 'luke', 'oli'),
      expected: true,
    },
  ];

  // === Run Tests ===
  const results: Map<string, { fullTrace: TestResult; incremental: TestResult }> =
    new Map();

  for (const mode of [ValidationMode.INCREMENTAL, ValidationMode.FULL_TRACE]) {
    console.log(`\n${'='.repeat(70)}`);
    console.log(`Mode: ${mode}`);
    console.log('='.repeat(70));

    const validator = new AspValidator(aspPath, 'tb', mode);

    for (const test of testCases) {
      const result = await test.validate(validator);
      const passed = result.valid === test.expected;

      const testResult: TestResult = {
        name: test.name,
        expected: test.expected,
        result,
        passed,
      };

      if (!results.has(test.name)) {
        results.set(test.name, { fullTrace: testResult, incremental: testResult });
      }

      if (mode === ValidationMode.INCREMENTAL) {
        results.get(test.name)!.incremental = testResult;
      } else {
        results.get(test.name)!.fullTrace = testResult;
      }

      const status = passed ? '✓' : '✗';
      const validStr = result.valid ? 'VALID' : 'INVALID';
      console.log(
        `\n${status} ${test.name}`
      );
      console.log(`  Expected: ${test.expected ? 'VALID' : 'INVALID'}, Got: ${validStr}`);
      console.log(`  Time: ${result.elapsedMs.toFixed(0)}ms`);
      if (result.error) {
        console.log(`  Error: ${result.error}`);
      }
    }
  }

  // === Summary ===
  console.log('\n' + '='.repeat(70));
  console.log('COMPARISON SUMMARY');
  console.log('='.repeat(70));
  console.log();
  console.log(
    'Test'.padEnd(50) +
      'Full-Trace'.padEnd(12) +
      'Incremental'.padEnd(12) +
      'Speedup'
  );
  console.log('-'.repeat(70));

  let allPassed = true;
  let totalSpeedup = 0;
  let speedupCount = 0;

  for (const [name, { fullTrace, incremental }] of results) {
    const ftTime = fullTrace.result.elapsedMs;
    const incTime = incremental.result.elapsedMs;
    const speedup = ftTime / incTime;

    const ftStatus = fullTrace.passed ? `${ftTime.toFixed(0)}ms ✓` : `FAIL ✗`;
    const incStatus = incremental.passed ? `${incTime.toFixed(0)}ms ✓` : `FAIL ✗`;

    console.log(
      name.substring(0, 48).padEnd(50) +
        ftStatus.padEnd(12) +
        incStatus.padEnd(12) +
        `${speedup.toFixed(1)}x`
    );

    if (!fullTrace.passed || !incremental.passed) {
      allPassed = false;
    }

    // Only count speedup if both modes gave correct answer
    if (fullTrace.passed && incremental.passed) {
      totalSpeedup += speedup;
      speedupCount++;
    }
  }

  console.log('-'.repeat(70));

  if (speedupCount > 0) {
    const avgSpeedup = totalSpeedup / speedupCount;
    console.log(`\nAverage speedup: ${avgSpeedup.toFixed(1)}x`);
  }

  console.log();
  if (allPassed) {
    console.log('✓ ALL TESTS PASSED');
    console.log('✓ TypeScript incremental mode at feature parity with Python');
  } else {
    console.log('✗ SOME TESTS FAILED');
  }

  // Verify both modes agree
  let modesAgree = true;
  for (const [name, { fullTrace, incremental }] of results) {
    if (fullTrace.result.valid !== incremental.result.valid) {
      console.log(`\n✗ MISMATCH: ${name}`);
      console.log(`  Full-trace: ${fullTrace.result.valid}`);
      console.log(`  Incremental: ${incremental.result.valid}`);
      modesAgree = false;
    }
  }

  if (modesAgree) {
    console.log('\n✓ Both modes produce identical results');
  }
}

// Run the tests
runComparisonTests().catch(console.error);

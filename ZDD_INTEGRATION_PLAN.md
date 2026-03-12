# botc-zdd- Integration Plan

This document captures the multi-prompt plan for building the `botc-zdd-` library
(a dependency of botc-asp) and reviewing its PRs to ensure they match our integration
expectations.

## Overview

The botc-zdd- library provides ZDD-based (Zero-suppressed Decision Diagram) world tracking
for Blood on the Clocktower game state. It encodes role distributions, seat assignments,
and night information as ZDD phases, allowing efficient enumeration and constraint propagation.

We (botc-asp) will eventually integrate this library to provide a web-based BotC solver
alongside our existing ASP-based approach.

---

## Prompt 1: Phase-Chain Architecture + Night 1 Info Roles (PRs #2, #3, #4)

**Status: Merged**

Established the core architecture:
- ZDD class with union, product, require, offset, count, enumerate operations
- Phase chain: Distribution -> SeatAssignment -> NightInfo
- Game class managing phase transitions
- Night 1 info roles: Washerwoman, Librarian, Investigator, Chef, Empath
- Variable encoding: each info output gets a unique variable ID
- exactlyOne ZDD constraint for each info role's valid outputs
- Cross-product of independent info roles
- Lookup helpers: findPairInfoVariable, findCountInfoVariable
- Observation system: require-variable, exclude-variable

---

## Prompt 2: Poisoner Target Selection and Malfunctioning Info Roles (PR #5)

**Status: Open (CI passing)**
**Branch: claude/add-poisoner-malfunctioning-roles-bDI3I**

### Requirements

1. **Architecture fix**: Malfunctioning info roles must generate the SAME variables as
   functioning ones, but allow ANY valid output (not just truthful). Previously they
   returned emptyResult() with zero variables.

2. **Poisoner target selection**: When Poisoner is in play, add N-1 target variables
   (one per non-Poisoner seat) with exactlyOne constraint. Variables come first (lower IDs),
   then info role variables follow. All in the same ZDD phase.

3. **Branching structure**: The valid info outputs DEPEND on which seat is poisoned, so
   this is NOT a simple cross-product. Build separate ZDD branches per poisoner target,
   then union them together.

4. **Drunk handling**: Respect `malfunctioningSeats` set (caller provides it). A seat in
   this set is always malfunctioning regardless of poisoner target.

5. **New types**: PoisonerTargetOutput, findPoisonerTargetVariable helper.

6. **Tests**: Poisoner targeting each role type, branch counts, observation forcing
   poisoner target, Drunk always-malfunctioning, combined Drunk+Poisoner.

### What NOT to do
- No Spy, Recluse, Fortune Teller
- No demon kill or death
- No distribution/seat assignment changes
- No Game class API changes beyond malfunctioningSeats parameter

---

## Prompt 3: Fortune Teller + Undertaker (Future)

Night 1 and recurring night info roles. Fortune Teller has a "red herring" (a good
player who registers as the Demon to the FT). Undertaker learns the role of the
executed player on the previous day.

---

## Prompt 4: Day Phase + Nominations + Execution (Future)

Model the day phase: nominations, voting, execution. Track alive/dead state.
Support the Slayer's day action.

---

## Prompt 5: Demon Kill + Night Order (Future)

Model the Imp's night kill. Handle the full night order: Poisoner -> info roles ->
Imp kill. Track death and its effect on subsequent night info (Empath's living
neighbors change).

---

## PR Review Log

### PR #5 Review (2026-03-12)

**Verdict: Approve with minor notes**

The implementation correctly addresses all requirements from Prompt 2:

**Architecture (correct):**
- Malfunctioning roles now generate the same variable IDs as functioning ones
- Uses a "maximal variable set" strategy: build with all seats malfunctioning to get
  the superset of variables, then constrain per-branch
- `buildAllInfoRolesConstrained` + `buildFunctioningRoleConstrained` filter truthful
  variables from the maximal set

**Poisoner variables (correct):**
- N-1 target variables allocated at IDs 0..N-2
- Info role variables start at ID N-1 (after poisoner vars)
- exactlyOne constraint on poisoner targets (implicit via singleSet per branch + union)

**Branching (correct):**
- Each poisoner target gets its own info-role ZDD
- Per branch: target seat added to malfunctioningSeats -> that role unconstrained
- Branches combined via union, not cross-product

**Drunk/malfunctioningSeats (correct):**
- `baseMalfunctioning` from config flows into every branch
- A seat in baseMalfunctioning is always unconstrained, even when poisoner targets elsewhere

**Variable consistency across branches (correct):**
- All branches share the same variable IDs from the maximal result
- The `pairOutputs`/`countOutputs` maps in the final result are from the maximal build (superset)

**Test coverage (comprehensive):**
- All no-Poisoner tests updated to use Spy (avoids triggering Poisoner logic)
- Per-branch count verification: 78 + 36 + 18 + 6 = 138
- Observation forcing poisoner target (require count=0 -> must target Chef)
- WW output only valid when malfunctioning -> forces poisoner on WW seat
- Drunk always unconstrained regardless of poisoner target
- Combined Drunk+Poisoner: 234 + 108 + 18 + 18 = 378
- Game class integration tests with malfunctioningSeats pass-through
- End-to-end pipeline with Poisoner

**Minor notes (non-blocking):**
1. The "No Outsiders" variable identification in `buildFunctioningRoleConstrained` (line 792)
   relies on it being the only variable without a pairOutputs entry. This works because
   `buildMalfunctioningPairRoleInfo` creates the "No Outsiders" var first (before pair vars)
   and doesn't add it to pairOutputs. Fragile but correct for now.

2. The Chef's maximal variable count is `numPlayers + 1` (counts 0..numPlayers), which is
   the same whether functioning or malfunctioning. Good - no variable count mismatch.

3. CI passes on this branch.

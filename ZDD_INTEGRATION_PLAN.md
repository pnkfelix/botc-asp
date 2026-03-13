# botc-zdd- Integration Plan

This document tracks the incremental development of the `botc-zdd-` library
and its integration into botc-asp. Each "Prompt" maps to one or more PRs on
botc-zdd- that we review before merging, then test integration with our ASP code.

## Key Architectural Insight

The ZDD phases are **not** one giant cross-product of all possibilities. Each
phase commits to concrete choices from the previous phase, then the ZDD
represents uncertainty only about the current phase's decisions. This mirrors
actual gameplay — there's one true game state, and the tool helps explore
what's consistent with observations.

---

## Prompt 1: Night 1 Information Roles (all functioning)

**Status: Merged (PRs #2, #3, #4)**

Takes a concrete seat assignment. Builds a ZDD of valid ST information choices
for Washerwoman, Librarian, Investigator, Chef, Empath. Chef and Empath are
fully determined by the seating (single valid output). Washerwoman/Librarian/
Investigator have ST choice (which pair to show), so the ZDD represents those
choices.

Established:
- ZDD class with union, product, require, offset, count, enumerate
- Phase chain: Distribution -> SeatAssignment -> NightInfo
- Game class managing phase transitions
- Variable encoding: each info output gets a unique variable ID
- exactlyOne ZDD constraint per info role
- Cross-product of independent info roles
- Lookup helpers: findPairInfoVariable, findCountInfoVariable
- Observation system: require-variable, exclude-variable

---

## Prompt 2: Poisoner + Drunk (malfunctioning info)

**Status: Merged (PR #5). Cross-validation passing.**

The poisoner's target choice is part of the Night 1 phase — it happens before
info roles act. This expands the Night 1 ZDD: for each poisoner target choice,
the targeted info role (if any) gets unconstrained outputs while others stay
truthful. Drunk is simpler — the Drunk's seat is known from the concrete
assignment, so they're just always malfunctioning. This prompt revises the night
phase builder from Prompt 1, adding poisoner target variables before info output
variables.

---

## Prompt 3: Spy/Recluse registration + Fortune Teller red herring

**Status: Merged (PRs #6, #8). Cross-validation switched to ASP oracle.**

These modify what counts as "truthful" for info roles. Spy can appear as good to
info roles, Recluse can appear as evil. Fortune Teller has a pre-designated red
herring player. All three expand the set of valid ST info choices without changing
the phase architecture.

Key changes:
- Spy registers as any Townsfolk/Outsider for info role purposes
- Recluse registers as any Minion/Demon for info role purposes
- Fortune Teller red herring: one non-Demon player designated to trigger "Yes"
- FT pair choice variables added (which two players the FT chooses to read)
- PR #8 fix: Spy/Recluse cannot register as their own type, and Librarian
  correctly handles optional "No Outsiders" branch

After this prompt landed, the Python oracle in `validate_night_info.py` was
replaced with the real clingo ASP oracle — two fully independent implementations
now cross-validate against each other. All 11 scenarios pass.

---

## Prompt 4: Night 2 active roles + death

**Status: Merged (PRs #9, #10). Cross-validation passing.**

Adds Night 2 action phase as a new ZDD phase type (`NightAction`), separate from
Night 1 info (`NightInfo`). Introduces:

- **Poisoner N2 retarget**: Poisoner picks a new target for Night 2
- **Monk protection**: Monk chooses one player to protect from demon kill
- **Imp kill**: Imp chooses a target; kill resolves considering Monk protection
- **Imp starpass**: Imp can self-target if living minions exist, passing demon
  status to a minion (recipient choice is a ZDD variable)
- **Soldier immunity**: Soldier is immune to demon kill (functioning only)
- **Empath N2**: Re-queries after death, skipping dead neighbors
- **Fortune Teller N2**: Re-queries after death, checking for new demon after
  starpass

Architecture: cascading branches — each Poisoner target creates a malfunctioning
context, each Monk target creates a protection context, each Imp target resolves
kill/no-kill, then info roles build on top. Branches combined via union.

PR #10 fix: Fortune Teller correctly detects starpass (demon changed) and Soldier
immunity prevents kill when functioning (not poisoned).

---

## Prompt 6: Day phase + Undertaker + dead seat handling

**Status: Merged (PR #11). Cross-validation passing.**

Adds Day phase support to the Game class and implements Undertaker as the first
Night 2 info role that depends on Day phase state.

Key changes:
- **`DayResult` type**: Records execution (seat, role), other deaths, cumulative
  dead set
- **`recordDay()` method**: State transition with zero ZDD variables (root = TOP),
  validates alive, snapshots for undo
- **`recordNightDeath()` method**: Records observed night deaths without creating
  a new phase
- **Dead seat handling**: Dead actors skipped (Poisoner, Monk, Imp), dead seats
  excluded from targets, dead minions excluded from starpass, Empath/FT skip
  pre-dead neighbors
- **Undertaker**: Learns executed player's role; one variable per role in
  selected set; functioning → exact role, malfunctioning → any role; inactive
  when dead or no execution
- **Undo support**: Restores dead set from snapshot, pops day results

Test coverage: 20 tests (17 specified + 3 integration).

---

## Prompt 7: Ravenkeeper, Scarlet Woman, Saint, Slayer

**Status: Merged (PR #12). CI passing.**

Rounds out the TB roles that interact with the existing phase structure:

- **Ravenkeeper**: Death-triggered Night 2 info role. When killed by Imp, wakes
  and chooses a player to learn their role. Integrated into night-action.ts
  branch logic with maximal variable allocation (TOP when not fired).
- **Scarlet Woman**: Demon succession in recordDay(). Imp executed with 5+ alive
  → living SW becomes new Imp. Reversible on undo. Also: functioning SW has
  mandatory starpass precedence over other minions.
- **Saint**: Game-ending condition. Functioning Saint executed → evil wins. Check
  in recordDay() using _malfunctioningSeats.
- **Slayer**: ZDD-based day ability. recordDay() builds exactlyOne ZDD of legal
  (target, outcome) pairs. Recluse gets both died/survived outcomes (ST choice).
  Undo restores _slayerUsed flag.

Test coverage: 23 tests. CI passes on all 4 commits.

---

## Prompt 8: Browser bundle + observation API

**Status: Merged (PR #13). CI passing.**

Prepares botc-zdd- for web UI integration with two deliverables:

**Browser bundle:**
- `npm run build:browser` → `dist/botc-zdd.esm.js` via esbuild
- No Node-only APIs (no fs/path/process)
- `test-browser.html` smoke test: 5-player game, WW observation, Chef query,
  undo verification. Works with `npx serve .`

**GameObserver class:**
- Wraps Game with high-level observation methods that translate human-readable
  observations into ZDD require/exclude operations
- Night 1: observePairInfo, observeCountInfo, observeFortuneTellerInfo,
  observeLibrarianNoOutsiders
- Night 2: observeNightDeath, observeEmpathN2, observeFortuneTellerN2,
  observeUndertakerRole, observeRavenkeeperInfo
- Day: observeExecution, observeNoExecution, observeSlayerShot
- Queries: worldCount(), possibleValues(role, nightNumber) with human-readable
  value descriptions and per-value world counts
- Undo: per-observation undo stack with rollback on inconsistent observations
- Error handling: inconsistent observations throw without modifying state

Test coverage: 10 tests. All pass.

---

## Prompt 5: Web UI integration (botc-asp side)

**Status: Not started — NEXT**

This is work on the botc-asp repo, not botc-zdd-. The ZDD library is now
feature-complete with a browser ESM bundle and observation API. The remaining
task is wiring it into the botc-asp web UI as a shadow solver alongside clingo.

Scope:
- Add botc-zdd ESM bundle to web-ui (vendor dist/botc-zdd.esm.js or npm link)
- Create Zdd.purs + Zdd.js FFI module (parallel to Clingo.purs + Clingo.js)
- Translate Grimoire state (seat assignments, reminder tokens) into GameObserver
  calls
- Display ZDD world count alongside clingo model count
- Optional: engine selector (Clingo / ZDD / Both) in game config UI
- Optional: possibleValues display for each info role

This is the final integration step — after this, the web UI runs both solvers
and users can compare results.

---

## Integration Milestones

- **Prompts 1-3**: Minimum for useful integration (info roles with full TB complexity) — **DONE**
- **Prompt 3 specifically**: Enables switching cross-validation to the real ASP oracle — **DONE**
- **Prompt 4**: Enables multi-night games — **DONE**
- **Prompt 6**: Day phase + Undertaker, completing the Night 1 → Day 1 → Night 2 loop — **DONE**
- **Prompt 7**: Remaining TB roles (Ravenkeeper, SW, Saint, Slayer) — **DONE**
- **Prompt 8**: Browser bundle + observation API (botc-zdd- side complete) — **DONE**
- **Prompt 5**: Web UI integration (botc-asp side) — **NEXT**

---

## Cross-Validation Strategy

### Current state (Prompts 1-4, 6 complete)

Cross-validation uses the **real clingo ASP oracle** for Night 1 info. The Python
oracle was retired after Prompt 3 landed. Two independent implementations (ASP
declarative rules vs ZDD procedural builder) are compared by output count.

**Night 1 info** (`cross-validation/validate_night_info.py`):
- 11 scenarios covering 5p–7p games with Poisoner, Spy, Recluse, Fortune Teller
- All pass: ASP projected model count == ZDD world count
- ZDD is 130x–8,000x faster than clingo across all scenarios

**Distributions** (`cross-validation/validate_distributions.py`):
- 12 scenarios (5p–10p, with/without Baron)
- All pass

**Benchmarks** (`cross-validation/benchmark_night_info.py`):
- ZDD: sub-2ms per scenario, 5 MB heap, max 510 nodes
- ASP: 250ms–5s per scenario, 70 MB peak RSS
- Largest scenario (7p Spy+info, 34,020 worlds): 0.6ms ZDD vs 5.0s ASP

### Future: Night 2 cross-validation

Night 2 action phase cross-validation is not yet implemented. This would require
extending the ASP oracle to model Night 2 actions (Poisoner retarget, Monk
protection, Imp kill, Empath/FT re-query, Undertaker) and comparing against the
ZDD `buildNightActionZDD` output. This is a natural Prompt 5 task.

---

## PR Review Log

### PR #5 Review (2026-03-12)

**Verdict: Approve with minor notes**

The implementation correctly addresses all Prompt 2 requirements:

**Architecture (correct):**
- Malfunctioning roles now generate the same variable IDs as functioning ones
- Uses a "maximal variable set" strategy: build with all seats malfunctioning to
  get the superset of variables, then constrain per-branch
- `buildAllInfoRolesConstrained` + `buildFunctioningRoleConstrained` filter
  truthful variables from the maximal set

**Poisoner variables (correct):**
- N-1 target variables at IDs 0..N-2
- Info role variables start after poisoner vars
- exactlyOne on poisoner targets (implicit via singleSet per branch + union)

**Branching (correct):**
- Each poisoner target gets its own info-role ZDD
- Per branch: target seat added to malfunctioningSeats -> role unconstrained
- Branches combined via union, not cross-product

**Drunk/malfunctioningSeats (correct):**
- `baseMalfunctioning` from config flows into every branch
- A seat in baseMalfunctioning is always unconstrained regardless of poisoner target

**Variable consistency across branches (correct):**
- All branches share the same variable IDs from the maximal result
- `pairOutputs`/`countOutputs` maps from the maximal build (superset)

**Test coverage (comprehensive):**
- No-Poisoner tests updated to use Spy (avoids triggering Poisoner logic)
- Per-branch count verification: 78 + 36 + 18 + 6 = 138
- Observation forcing poisoner target (require count=0 -> must target Chef)
- WW output only valid when malfunctioning -> forces poisoner on WW seat
- Drunk always unconstrained regardless of poisoner target
- Combined Drunk+Poisoner: 234 + 108 + 18 + 18 = 378
- Game class integration with malfunctioningSeats pass-through
- End-to-end pipeline with Poisoner

**Minor notes (non-blocking):**
1. "No Outsiders" variable identification in `buildFunctioningRoleConstrained`
   relies on it being the only variable without a pairOutputs entry. Fragile
   but correct for now.
2. Chef maximal variable count is `numPlayers + 1` whether functioning or not.
   No variable count mismatch.
3. CI passes.

### PR #6 Review (2026-03-12)

**Verdict: Approve — Prompt 3 complete**

Adds Spy/Recluse registration and Fortune Teller with red herring (+1,485 -373
across 5 files). Major rework of the night info builder:

- Spy registers as any Townsfolk/Outsider; Recluse as any Minion/Demon
- Fortune Teller: red herring seat config, FT pair choice variables, "Yes"/"No"
  output based on whether either chosen player is Demon (or red herring)
- Comprehensive test suite covering Spy/Recluse/FT interactions
- Cross-validated against clingo ASP oracle: all scenarios pass

### PR #8 Review (2026-03-12)

**Verdict: Approve — Prompt 3 bugfix**

Fixes two issues found during cross-validation (+77 -17 across 3 files):

1. Spy cannot register as Minion/Demon (own type), Recluse cannot register as
   Townsfolk/Outsider (own type) — this was producing extra worlds
2. Librarian correctly handles "No Outsiders" output as an optional branch when
   no Outsiders are in the game

After this fix, all 11 cross-validation scenarios pass against the ASP oracle.

### PR #9 Review (2026-03-12)

**Verdict: Approve — Prompt 4 complete**

Adds Night 2 action phase (+1,748 lines, 4 new files). Entirely new module
`night-action.ts` with cascading branch architecture:

- Variables: PoisonerN2 targets, MonkTarget, ImpTarget, StarpassRecipient,
  EmpathN2, FortuneTellerN2
- Kill resolution: Imp target dies unless Monk-protected or Soldier (functioning)
- Starpass: Imp self-targets → dies, demon status passes to living minion
- Empath/FT re-query after death with updated neighbor/demon state
- 37 tests covering all role interactions and branch counts

### PR #10 Review (2026-03-12)

**Verdict: Approve — Prompt 4 bugfix**

Fixes two issues (+299 -13 across 2 files):

1. Fortune Teller starpass detection: FT now correctly identifies the new demon
   seat after starpass (was using original demon seat)
2. Soldier immunity: Soldier is immune to demon kill when functioning (not
   poisoned by Night 2 Poisoner target)

### PR #11 Review (2026-03-12)

**Verdict: Approve — Prompt 6 (Day phase + Undertaker)**

Adds Day phase support and Undertaker role (+896 -31 across 4 files):

**Day phase (Game class):**
- `DayResult` interface: dayNumber, executedSeat/Role, otherDeaths, deadSeats
- `recordDay()`: zero ZDD variables (root = TOP), validates alive, snapshots for undo
- `recordNightDeath()`: state update without new phase
- Undo restores dead set from snapshot

**Dead seat handling (night-action.ts):**
- Dead Poisoner/Monk/Imp skipped (no variables generated)
- Dead seats excluded from all target lists
- Dead minions excluded from starpass eligibility
- Empath/FT skip pre-dead seats in neighbor calculations

**Undertaker:**
- One variable per role in selectedRoles
- Active only when alive AND execution occurred
- Functioning: exactly the executed role; malfunctioning: any role
- Integrated into `buildInfoRolesForBranch` via `zdd.product`

**Tests:** 20 tests (5 Day phase, 5 dead seats, 5 Undertaker, 2 branch counts,
3 Game class integration). All pass.

**Cross-validation:** All 11 Night 1 info scenarios and 12 distribution scenarios
continue to pass. ZDD performance unchanged (sub-2ms, 5 MB heap).

### PR #12 Review (2026-03-13)

**Verdict: Approve — Prompt 7 (Ravenkeeper, SW, Saint, Slayer)**

Adds four TB roles (+1,097 -20 across 4 files, 4 commits, 23 tests):

**Ravenkeeper (night-action.ts):**
- Death-triggered: `buildRavenkeeperForBranch` returns TOP when RK not killed
- Functioning: union of {targetVar(t)} × {roleVar(trueRole(t))} per target
- Malfunctioning: exactlyOne(targets) × exactlyOne(roles)
- Maximal variable allocation, per-branch TOP when inactive

**Scarlet Woman (game.ts):**
- Promotion in recordDay(): checks RoleType.Demon + aliveCount >= 5
- Undo reverses promotion by restoring "Scarlet Woman" role
- Starpass precedence (night-action.ts): functioning SW is mandatory recipient

**Saint (game.ts):**
- recordDay() check: executedRole === "Saint" && !malfunctioning → evil wins
- GameOverResult type on DayResult

**Slayer (game.ts — refactored from imperative to ZDD):**
- Day phase ZDD variables: one per legal (target, outcome) pair
- Recluse: both died/survived outcomes (registersAs Demon check)
- Undo restores _slayerUsed flag
- recordDay() overloaded to accept slayerShot option

### PR #13 Review (2026-03-13)

**Verdict: Approve — Prompt 8 (Browser bundle + observation API)**

Adds browser bundle and GameObserver (+1,084 -108 across 6 files):

**Browser bundle:**
- `build:browser` script: esbuild → dist/botc-zdd.esm.js
- test-browser.html: 5-player smoke test with observation + undo

**GameObserver (src/observer.ts, 527 lines):**
- Wraps Game, translates observations to require/exclude via find*Variable helpers
- Night 1: observePairInfo, observeCountInfo, observeFortuneTellerInfo,
  observeLibrarianNoOutsiders
- Night 2: observeEmpathN2, observeFortuneTellerN2, observeUndertakerRole,
  observeRavenkeeperInfo (two requires: target + role)
- Day: observeExecution, observeNoExecution, observeSlayerShot
- possibleValues: iterates role variables, requires each temporarily, returns
  human-readable {value, worldCount} entries
- Undo: per-observation stack, rollback on inconsistent observations
- Error handling: BOTTOM check → restore previous root → throw

**Minor notes (non-blocking):**
1. Ravenkeeper undo pushes two entries (target + role) — caller must undo twice
2. observeNightDeath not on undo stack (state transition, not ZDD observation)
3. undo() uses private member cast for root restoration
4. 10 tests covering possibleValues sums, observation narrowing, conflicts,
   undo, full pipeline, Night 2 integration

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

**Status: Not started**

These modify what counts as "truthful" for info roles. Spy can appear as good to
info roles, Recluse can appear as evil. Fortune Teller has a pre-designated red
herring player. All three expand the set of valid ST info choices without changing
the phase architecture.

---

## Prompt 4: Active night roles + death

**Status: Not started**

Imp demon kill (not Night 1), Monk protection, Soldier immunity. These add
variables to later night phases and introduce the concept of players dying between
nights, which affects Empath neighbor calculations and game progression.

---

## Prompt 5: Integration readiness

**Status: Not started**

Clean API surface. Probably a high-level "shadow mode" function that takes the
same inputs the botc-asp web app works with and produces comparable outputs.
Verify browser bundleability. Maybe a small cross-validation script like we have
for distributions.

---

## Integration Milestones

- **Prompts 1-3**: Minimum for useful integration (info roles with full TB complexity)
- **Prompt 3 specifically**: Enables switching cross-validation to the real ASP oracle (see below)
- **Prompt 4**: Enables multi-night games
- **Prompt 5**: Bridge back to this repo

---

## Cross-Validation Strategy

### Current state (Prompts 1-2)

The night info cross-validation (`cross-validation/validate_night_info.py`) uses
a **Python oracle** that independently computes expected world counts. This is
NOT ideal — it reimplements the same spec as the ZDD, so shared misunderstandings
of the spec would not be caught. We use it because the ZDD doesn't yet model
Spy/Recluse registration or Fortune Teller, so running clingo against the full
botc-asp rules would produce different (larger) answer set counts.

Distribution cross-validation already uses the real clingo oracle and passes.

### Target state (after Prompt 3)

Once Prompt 3 lands (Spy/Recluse registration + Fortune Teller red herring), the
ZDD's modeling scope for Night 1 info will match what the botc-asp ASP rules
encode. At that point, **replace the Python oracle with clingo enumeration
against the real ASP rules**:

1. Feed clingo the concrete seat assignment + `botc.lp` + `tb.lp` + role files
2. Enumerate all valid `st_tells_core` answer sets (including poisoner target,
   malfunctioning outputs, Spy/Recluse registration, FT red herring)
3. Count answer sets and compare against ZDD world count

This is the real validation — two independent implementations (ASP declarative
rules vs ZDD procedural builder) of the same game logic, compared by output.
Any disagreement reveals a genuine bug in one or the other.

**Priority: Switch to the ASP oracle as soon as Prompt 3 merges.** The Python
oracle is a stopgap. Do not let it persist beyond Prompt 3.

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

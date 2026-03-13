# Prompt 4: Night 2+ Actions — Demon Kill, Monk Protection, Death

## Context

Prompts 1–3 built the Night 1 info phase: for a concrete seat assignment, the
ZDD represents all valid Storyteller information choices (Washerwoman, Librarian,
Investigator, Chef, Empath, Fortune Teller) accounting for Poisoner/Drunk
malfunctioning and Spy/Recluse registration.

Prompt 4 extends the game into **Night 2+**, adding the first "action" roles —
roles whose choices change game state rather than just producing information.

## Goal

Add a new phase builder for **Night 2+ actions** that, given a concrete seat
assignment and a concrete Night 1 info result, builds a ZDD representing all
valid Storyteller + player choice combinations for Night 2. The key new
mechanics are:

1. **Poisoner retargets** (already exists as a concept — new target for Night 2)
2. **Monk protection** (new)
3. **Imp demon kill** (new)
4. **Death** (new — a player may die, changing the game state)
5. **Night 2 info roles** (Empath, Fortune Teller re-query with updated state)

## Night 2 Role Order

From the Trouble Brewing script, the Other Night order is:

| Order | Role             | Type           | Notes                           |
|-------|------------------|----------------|---------------------------------|
| 1     | Poisoner         | Choice         | Picks new poison target         |
| 2     | Monk             | Choice         | Picks player to protect (not self) |
| 3     | Scarlet Woman    | Automatic      | No player choice; just a timing slot |
| 4     | Imp              | Choice         | Picks kill target (can self-target = starpass) |
| 5     | Ravenkeeper      | Conditional    | Only acts if died tonight (skip for now) |
| 6     | Empath           | Info (count)   | Living evil neighbors (updated for deaths) |
| 7     | Fortune Teller   | Info (pair+yn) | Same as Night 1 but new FT choice |
| 8     | Undertaker       | Info           | Who was executed today (skip for now) |
| 9     | Butler           | Choice         | Skip for now                    |
| 10    | Spy              | Automatic      | No new variables needed         |

**In scope for this prompt:** Poisoner, Monk, Imp, Empath, Fortune Teller.
**Out of scope:** Ravenkeeper, Undertaker, Butler (these can be added later
without changing the architecture).

## Detailed Specifications

### Poisoner (Order 1)

The Poisoner picks a new target for Night 2. This works exactly like Night 1
Poisoner targeting — one variable per eligible target seat, exactlyOne
constraint. The Night 1 poison expires at the start of Night 2; the new target
becomes poisoned for Night 2's remaining roles.

**Variables:** One per player seat (excluding the Poisoner's own seat), same as
Night 1.

**Interaction:** The new poison target determines which roles malfunction during
Night 2 — this includes the Monk (a poisoned Monk's protection does nothing)
and info roles (poisoned Empath gets unconstrained output).

### Monk (Order 2) — NEW

The Monk chooses one player to protect from demon kill. Constraints:
- **Cannot protect self**
- The Monk must be alive at the start of Night 2
- If the Monk is **poisoned/drunk**, they still make a choice (they're woken up
  as normal), but the protection token has no effect

**Variables:** One variable per eligible target (all players except the Monk),
with an exactlyOne constraint.

**Effect:** If the Monk is functioning, the protected player survives demon
kill. If the Monk is malfunctioning (poisoned/drunk), the choice is made but
protection does not apply.

**Branching pattern:** Similar to Poisoner — for each Poisoner target branch,
if the Monk is the poison target, the Monk's protection is nullified. Cross
these branches with Monk target choices.

### Imp Kill (Order 4) — NEW

The Imp chooses one player to kill. Constraints:
- Can target **any** player, including itself (starpass)
- **Starpass** (self-target) is only legal if at least one living minion exists
  to receive the demon role
- The Imp must be alive and functioning to place the kill token
- If the Imp is poisoned, they still choose a target but nobody dies

**Variables:** One variable per target player, exactlyOne constraint.

**Kill resolution:** After the Imp's choice, the kill succeeds unless:
1. The target is protected by a functioning Monk, OR
2. The Imp is malfunctioning (poisoned/drunk)

If the kill succeeds, the target **dies**. This affects subsequent info roles
in the same night.

### Starpass Sub-choices — NEW

When the Imp targets itself and the kill succeeds:
1. The Imp dies
2. The ST chooses one living minion (from initial minion assignments) to become
   the new Imp
3. The chosen minion's role changes to Imp immediately

**Variables:** When starpass is possible, add one variable per eligible minion
recipient, with an exactlyOne constraint (only within the starpass sub-branch).

**Note:** Starpass is a relatively rare edge case. It's fine to handle it as
additional branches within the Imp's target selection. If starpass is impossible
(no living minions), the Imp cannot self-target — that target variable is
excluded entirely.

### Death State — NEW

This is the key architectural addition. After the Imp's action resolves:
- If the kill succeeded, exactly one player is now dead
- If the kill failed (Monk protection or Imp poisoned), nobody new is dead

The death state affects:
1. **Empath neighbor calculation** — dead players are skipped when finding
   living neighbors
2. **Fortune Teller choices** — dead players are still valid FT targets (the FT
   picks any two players regardless of alive/dead status)
3. **Future nights** — dead players don't wake up (not relevant for this prompt,
   but the architecture should support it)

**Encoding suggestion:** You don't necessarily need explicit "death" variables.
Since the Imp target + Monk protection + poison status fully determine who dies,
the death outcome is implicit in the branch structure. Each branch of the ZDD
already knows "in this world, player X died" or "nobody died."

### Night 2 Empath (Order 6)

Same as Night 1 Empath, but:
- **Living neighbor calculation changes** if someone died this night
- If the Empath is the one who died, they don't wake up (no Empath output)
- If a neighbor of the Empath died, the Empath's living neighbors shift
- If the Empath is poisoned (Night 2 Poisoner target), output is unconstrained

**Key complexity:** The Empath's correct output depends on the kill outcome,
which depends on the Imp target × Monk protection × Poisoner target
interaction. Each branch of the ZDD may produce a different Empath count.

### Night 2 Fortune Teller (Order 7)

Same as Night 1 FT, but:
- The FT makes a **new** pair choice for Night 2 (independent of Night 1)
- The **red herring is fixed** for the whole game (chosen during setup/Night 1)
- If the FT died this night, no FT output
- If the FT is poisoned, output is unconstrained

### Soldier Immunity (Stretch Goal)

The Soldier is immune to demon kill when functioning. Not yet implemented in
the ASP either (`soldier.lp` is a TODO stub), but the architecture should
make it easy to add later:
- If the Imp targets the Soldier and the Soldier is functioning → kill fails
- If the Soldier is poisoned → immunity lost, kill succeeds

This is structurally similar to Monk protection — it adds another condition
to the "does the kill succeed?" check. Can be a follow-up PR.

## Architectural Guidance

### Branching Structure

Night 2 has a **cascade of dependent choices**:

```
Poisoner target (N-1 branches)
  └─ Monk target (N-1 branches, but Monk malfunctions if poisoned)
       └─ Imp target (N branches, starpass conditional)
            └─ Kill outcome (determined by above)
                 └─ Empath output (depends on who died)
                 └─ FT pair choice + output (depends on who died)
```

This is more complex than Night 1's branching (which was just Poisoner ×
FT red herring). You'll likely need to:

1. Build the choice variables for Poisoner, Monk, Imp in sequence
2. For each combination, determine the kill outcome
3. Build info role outputs conditional on the outcome
4. Union all branches

**Optimization:** Many branches produce the same info role outputs (e.g., if
the Imp kills player 3 and the Empath's neighbors are players 1 and 5, the
Empath count is the same regardless of who the Monk protected). The ZDD's
structural sharing should compress these naturally.

### Variable Layout

Suggested variable ordering within the Night 2 phase:

```
[Poisoner targets] [Monk targets] [Imp targets] [Starpass recipients] [Empath outputs] [FT outputs]
```

All variables should be allocated upfront (maximal set), then constrained per
branch — same pattern as the existing Night 1 builder.

### Phase Integration

This should be a new phase in the Game class (e.g., `PhaseType.NightAction` or
`PhaseType.Night2`). It takes as input:
- The concrete seat assignment (from Phase 2)
- Malfunctioning seats from Night 1 (Drunk is always malfunctioning)
- Red herring designation (from Night 1 — carries forward)

The Game class should support building this phase after the Night 1 info phase.

### What NOT to Build

- **Day phase** (nominations, voting, execution) — that's a future prompt
- **Multi-night loop** (Night 3, 4, ...) — just Night 2 for now
- **Ravenkeeper/Undertaker/Butler** — can be added as incremental follow-ups
- **Scarlet Woman promotion** — only triggers on execution (day phase), so
  it doesn't affect Night 2 actions. But be aware it exists for later.
- **Game-over detection** — the ZDD doesn't need to check win conditions

## Cross-Validation

After implementation, the cross-validation script in `botc-asp/cross-validation/`
can be extended to verify Night 2 counts. The ASP oracle approach works the same
way: fix a seat assignment, project onto observable atoms (Poisoner target, Monk
target, Imp target, kill outcome, Empath output, FT output), and count models.

A good first test scenario:
- **5p: Monk, Chef, Empath, Poisoner, Imp** — Monk + Poisoner + Imp all active,
  Chef provides no Night 2 info (simplifies verification)
- **5p: Monk, Empath, FT, Poisoner, Imp** — adds FT Night 2 output

## Deliverables

1. Night 2 action builder function (analogous to `buildNightInfoZDD`)
2. Tests covering:
   - Monk protection blocks Imp kill
   - Poisoned Monk's protection has no effect
   - Poisoned Imp's kill has no effect
   - Imp starpass mechanics
   - Empath output changes based on who died
   - Branch count verification against hand-calculated expectations
3. Game class integration (new phase after NightInfo)
4. Exported types for new variable categories (MonkTarget, ImpTarget, etc.)

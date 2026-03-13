# Prompt 3: Spy/Recluse registration + Fortune Teller red herring

PR #5 established Poisoner target selection and malfunctioning info roles. Now add Spy/Recluse mis-registration effects on existing info roles, and add the Fortune Teller as a new info role with its red herring mechanic.

## Spy registration

The Spy is a Minion but can **register as** Townsfolk, Outsider, or Minion (NOT Demon) to info roles. The Spy can also register as Good or Evil for alignment-checking roles (Chef, Empath). Critically, the Spy can register DIFFERENTLY to each info role because they act at different times in the night order. Registration is a Storyteller choice, not a player choice.

### Effect on pair info roles (Washerwoman, Librarian, Investigator)

When functioning, the current code finds valid "reference targets" by checking `getRoleType(role, script) === targetType`. The Spy changes this: the Spy's seat is a valid reference target for roles checking Townsfolk (Washerwoman), Outsider (Librarian), AND Minion (Investigator).

Furthermore, when the Spy is the reference target, the ST can name **any role of the registering category from the script** — not just the Spy's actual role. This is because `may_register_as_role` for the Spy includes all roles of categories the Spy can register as.

Concretely:
- **Washerwoman**: Spy can be the reference. Valid outputs: `(spy_seat, decoy) × ALL 13 Townsfolk roles`. For a normal Townsfolk target like the Chef at seat 1, valid outputs are only `(1, decoy) × {"Chef"}`.
- **Librarian**: Spy can be the reference. Valid outputs: `(spy_seat, decoy) × ALL 4 Outsider roles`.
- **Investigator**: Spy is already a Minion, but the expansion still applies — the ST can name any Minion role, not just "Spy". So instead of `(spy_seat, decoy) × {"Spy"}`, it becomes `(spy_seat, decoy) × ALL 4 Minion roles`.

Note: the Spy can also be a **decoy** (the non-reference player in the shown pair). This is already handled — any non-info-role, non-reference seat can be a decoy. Nothing changes for decoys.

### Effect on Chef

The Chef counts adjacent evil pairs using alignment registration. The Spy can register as Evil or Good at Chef's time. This means the Chef's count is no longer fully determined by the seat assignment — it depends on the ST's registration choice for the Spy.

Compute all possible evil pair counts by considering the Spy registering as evil vs. good. The valid Chef outputs are the set of all achievable counts. For example, if the Spy is adjacent to the Imp:
- Spy registers evil → evil pair count includes (Spy, Imp)
- Spy registers good → that pair doesn't count
- So Chef has 2 valid counts instead of 1

### Effect on Empath

Same principle. If the Spy is a living neighbor of the Empath, the Spy can register as Evil or Good, changing the Empath's count. Compute all achievable counts and allow any of them.

## Recluse registration

The Recluse is an Outsider but can register as Outsider, Minion, or Demon (NOT Townsfolk). Can register as Good or Evil.

### Effect on pair info roles

- **Washerwoman**: Recluse CANNOT register as Townsfolk → no effect.
- **Librarian**: Recluse is already an Outsider, so it's already a valid target. But when the Recluse is the reference, the ST can name ANY Outsider role (not just "Recluse"). So Recluse expands Librarian outputs from `(recluse_seat, decoy) × {"Recluse"}` to `(recluse_seat, decoy) × ALL 4 Outsider roles`.
- **Investigator**: Recluse can register as Minion, so the Recluse becomes a valid reference target. Valid outputs: `(recluse_seat, decoy) × ALL 4 Minion roles`. This means a good player can appear as a minion to the Investigator.

### Effect on Chef and Empath

Same as Spy: Recluse can register as Evil, so compute all achievable evil pair counts / evil neighbor counts considering Recluse evil vs. good.

### Combined Spy + Recluse

When both Spy and Recluse are in play, their registration choices are independent. For Chef/Empath, enumerate all combinations (Spy evil/good × Recluse evil/good) and collect all achievable counts.

## Fortune Teller

The Fortune Teller is a new Night 1 info role (also acts on other nights, but that's Prompt 4's concern). The FT chooses two players each night and the ST tells them "Yes" or "No" — whether one of the chosen players is the Demon.

### Red herring setup

At game setup (before Night 1), the ST designates exactly one player as the Fortune Teller's "red herring." Constraints on the red herring:
- Cannot be the actual Demon (by role assignment, not registration)
- Must be a player who CAN register as Good (note: this is about eligibility, not a forced registration)

The red herring always pings as "the Demon" to the Fortune Teller, even though they aren't.

### Red herring variables

Add red herring designation variables to the Night 1 phase, analogous to poisoner target variables. One variable per eligible player. exactlyOne constraint. The eligible players are: everyone except the Demon seat and the FT seat.

Why exclude the FT seat? The FT can't choose themselves as one of the two players they ask about (the rules say the FT points at two other players), and the red herring only matters for FT queries, so placing it on the FT itself would have no gameplay effect. Actually — check whether the ASP rules allow the red herring on the FT. If they do, include the FT seat as eligible. The constraint is just: not the Demon.

(On reflection: the ASP rule is `not demon(R)` for the red herring player, and the red herring must register as good. The FT is a Townsfolk, so they always register as good. The ASP rules don't exclude the FT. So include the FT seat as eligible.)

Correction: eligible red herring seats = all seats except the Demon seat. (N-1 candidates.)

### FT output variables

For each possible pair of players the FT could choose (C(N-1, 2) pairs, excluding the FT from both positions), and for each answer (Yes/No), create a variable. The valid answers when functioning depend on:

- A chosen player "pings" if they register as Demon OR carry the red herring token
- If either chosen player pings → answer is Yes
- If neither pings → answer is No

When malfunctioning (poisoned/drunk), any (pair, answer) is valid.

### Branching for red herring

The FT outputs DEPEND on which player is the red herring, similar to how info outputs depend on which seat the Poisoner targets. For each possible red herring designation, the set of valid FT outputs changes (which pairs give "Yes" vs "No").

However, the red herring does NOT affect non-FT info roles (WW, Lib, Inv, Chef, Empath are unaffected). So the branching can be structured as:

1. Non-FT info roles: build once, independent of red herring
2. FT outputs: branch per red herring candidate, union the FT ZDDs
3. Red herring designation variable: one per branch (like poisoner target)
4. Cross-product: (non-FT info ZDD) × (union of red-herring × FT-output branches)

This layers on top of the existing Poisoner branching. The full branch structure when both Poisoner and FT are in play:

For each poisoner target:
  - Non-FT info roles with poisoner malfunctioning effect
  - If FT is poisoned (target == FT seat): FT unconstrained, red herring exactlyOne (doesn't constrain FT)
  - If FT is not poisoned: for each red herring candidate, build constrained FT outputs, union
  - Combine via product (non-FT × FT+redherring) then union across poisoner branches

### Variable ordering

Variable IDs should be ordered: poisoner targets (if any), red herring designation, FT outputs, then other info role outputs. Or: poisoner targets, other info role outputs, red herring designation, FT outputs. The key constraint is that variables within a dependent group are contiguous. Choose whichever ordering makes the implementation cleanest.

### New types

Add a `RedHerringOutput` type (analogous to `PoisonerTargetOutput`) with a `targetSeat` field. Add `redHerringOutputs: Map<number, RedHerringOutput>` to `NightInfoResult`. Add `findRedHerringVariable(result, targetSeat)` lookup helper.

Add FT to the pair/count info output types — the FT output is a (playerA, playerB, answer) triple, which could use a new `FortuneTellerOutput` type with `playerA`, `playerB`, `answer: "Yes" | "No"` fields, plus a lookup helper `findFortuneTellerVariable(result, playerA, playerB, answer)`.

## Role metadata changes

The `Role` interface needs a way to express registration capabilities. Add an optional `registersAs` field to `Role`:

```typescript
export interface Role {
  name: string;
  type: RoleType;
  distributionModifier?: DistributionModifier;
  /** Categories this role can register as (Spy, Recluse). If absent, registers only as actual type. */
  registersAs?: {
    roleTypes: RoleType[];  // e.g., Spy: [Townsfolk, Outsider, Minion]
    alignments: ("Good" | "Evil")[];  // e.g., Spy: ["Good", "Evil"]
  };
}
```

Update `TROUBLE_BREWING` to add this for Spy and Recluse:
- Spy: `registersAs: { roleTypes: [Townsfolk, Outsider, Minion], alignments: ["Good", "Evil"] }`
- Recluse: `registersAs: { roleTypes: [Outsider, Minion, Demon], alignments: ["Good", "Evil"] }`

The night info builder uses this metadata to determine: (a) which seats are valid reference targets for pair roles, (b) which role names can be shown for each reference target, and (c) what registration choices affect Chef/Empath counts.

## Testing

### Spy tests
- Spy in play, no Poisoner: WW valid outputs expand (Spy as Townsfolk reference with all 13 Townsfolk role names)
- Spy in play: Librarian outputs expand (Spy as Outsider reference with all 4 Outsider role names)
- Spy in play: Investigator outputs expand (Spy can be named as any Minion, not just "Spy")
- Spy adjacent to evil: Chef has 2 valid counts (Spy evil vs. good)
- Spy as Empath neighbor: Empath has 2 valid counts
- Spy + Poisoner: total world count = sum of per-branch counts with expanded outputs

### Recluse tests
- Recluse in play: Investigator sees Recluse as valid Minion reference
- Recluse in play: Librarian outputs expand (Recluse can be named as any Outsider)
- Recluse adjacent to evil: Chef count expands
- Recluse as Empath neighbor: Empath count expands

### Fortune Teller tests
- FT in play, no Poisoner: red herring variables present, FT output variables present
- FT with specific red herring: require red herring on seat X, verify FT outputs constrained
- FT with Demon in pair: always "Yes" regardless of red herring
- FT with red herring in pair: "Yes" regardless of actual demon
- FT with neither Demon nor red herring: "No"
- FT malfunctioning (poisoned): any pair+answer valid
- FT red herring + Poisoner: branching structure works

### Combined tests
- Spy + Recluse + FT + Poisoner: verify total world count
- Cross-validate total world counts against the Python oracle (I will update validate_night_info.py after this merges)

## What NOT to do

- Do not model the Undertaker (it's a "later night" role that requires day/execution, beyond Prompt 3's scope)
- Do not model other-night FT queries (Prompt 4)
- Do not add death or day phase mechanics
- Do not change distribution or seat assignment code
- Do not model Spy "seeing the Grimoire" ability (that's a different mechanic from registration)

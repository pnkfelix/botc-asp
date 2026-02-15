% Grandmother
% You start knowing a good player & their character. If the Demon kills them, you die too.

first_night_substep(grandmother, 1, st_places(gm_grandchild)).
first_night_substep(grandmother, 2, st_tells(gm_info)).

% Setup: ST places the grandchild token on exactly one player at night 1
{ reminder_on(gm_grandchild, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, _, grandmother).

% Constraint: grandchild must be actually good (not just register as good; separate from self-exclusion below)
:- reminder_on(gm_grandchild, P, _),
   not actual_category(P, good).

% Constraint: grandchild cannot be the Grandmother herself (separate from good-check: different condition)
:- reminder_on(gm_grandchild, P, _),
   assigned(0, P, grandmother).

% Functioning tell: Grandmother learns the grandchild's identity and true role (deterministic)
st_tells_core(grandmother, GM, info(Grandchild, R), T) :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, GM, grandmother),
    reminder_on(gm_grandchild, Grandchild, T),
    assigned(0, Grandchild, R),
    functioning(GM, T).

% Not-functioning tell: ST freely chooses any player/role pair (choice rule; can't merge with functioning)
{ st_tells_core(grandmother, GM, info(P, R), T) : player(P), role(R) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, GM, grandmother),
    not functioning(GM, T).

other_night_substep(grandmother, 1, st_tells(gm_status)).

% Grandmother dies when her grandchild is killed by a demon (co-death trigger)
grandmother_dies_from_grandchild(GM, T) :-
    assigned(0, GM, grandmother),
    reminder_on(gm_grandchild, Grandchild, T),
    died_from_demon(Grandchild, T),
    alive(GM, T).

% died_from_demon: uses the generic pipeline (no cycles — Grandmother doesn't feed back into demon kill)
died_from_demon(P, T) :- demon_kill_succeeds(P, T).

% Mark Grandmother as dead at the instant of the grandchild kill (mid-night state tracking)
dead_at_time(GM, T) :- grandmother_dies_from_grandchild(GM, T).
% Record Grandmother death at final night time for dawn processing (separate: different timing semantics)
died(GM, T) :- grandmother_dies_from_grandchild(GM, T), final_night_time(N, T), alive(GM, night(N, 0, 0)).

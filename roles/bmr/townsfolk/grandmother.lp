% Grandmother
% You start knowing a good player & their character. If the Demon kills them, you die too.

first_night_substep(grandmother, 1, st_places(gm_grandchild)).
first_night_substep(grandmother, 2, st_tells(gm_info)).

% ST chooses a good player to be the grandchild
{ reminder_on(gm_grandchild, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, _, grandmother).

% Grandchild must be a good player (not just register as good)
:- reminder_on(gm_grandchild, P, _),
   not actual_category(P, good).

% Grandchild cannot be the Grandmother herself
:- reminder_on(gm_grandchild, P, _),
   assigned(0, P, grandmother).

% ST tells the Grandmother who the grandchild is and their role
st_tells_core(grandmother, GM, info(Grandchild, R), T) :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, GM, grandmother),
    reminder_on(gm_grandchild, Grandchild, T),
    assigned(0, Grandchild, R),
    functioning(GM, T).

% If not functioning, ST can show any good player and any role
{ st_tells_core(grandmother, GM, info(P, R), T) : player(P), role(R) } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(grandmother, RoleOrd),
    assigned(0, GM, grandmother),
    not functioning(GM, T).

% Grandmother wakes other nights to check if grandchild is alive
other_night_substep(grandmother, 1, st_tells(gm_status)).

% If grandchild was killed by demon, Grandmother also dies
grandmother_dies_from_grandchild(GM, T) :-
    assigned(0, GM, grandmother),
    reminder_on(gm_grandchild, Grandchild, T),
    died_from_demon(Grandchild, T),
    alive(GM, T).

% Track deaths from demon (imp, zombuul, pukka, shabaloth, po)
died_from_demon(P, T) :- imp_kill_succeeds(P, T).
died_from_demon(P, T) :- zombuul_kill_succeeds(P, T).
died_from_demon(P, T) :- pukka_kill_succeeds(P, T).
died_from_demon(P, T) :- shabaloth_kill_succeeds(P, T).
died_from_demon(P, T) :- po_kill_succeeds(P, T).

% Grandmother death is announced at the same time as grandchild
dead(GM, T) :- grandmother_dies_from_grandchild(GM, T).
died(GM, T) :- grandmother_dies_from_grandchild(GM, T), final_night_time(N, T), alive(GM, night(N, 0, 0)).

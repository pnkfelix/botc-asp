% Chambermaid
% Each night, choose 2 alive players (not yourself): you learn how many woke tonight due to their ability.

first_night_substep(chambermaid, 1, st_asks(choose_two)).
first_night_substep(chambermaid, 2, player_chooses(target1)).
first_night_substep(chambermaid, 3, player_chooses(target2)).
first_night_substep(chambermaid, 4, st_tells(cm_info)).

other_night_substep(chambermaid, 1, st_asks(choose_two)).
other_night_substep(chambermaid, 2, player_chooses(target1)).
other_night_substep(chambermaid, 3, player_chooses(target2)).
other_night_substep(chambermaid, 4, st_tells(cm_info)).

% Night 1 target 1: separate from target 2 because they occupy different substeps
{ cm_choice(CM, P, 1, night(1, RoleOrd, 2)) : player(P), P != CM } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid).

% Night 1 target 2: separate slot for the second chosen player
{ cm_choice(CM, P, 2, night(1, RoleOrd, 3)) : player(P), P != CM } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid).

% Night 1: two targets must be different players
:- cm_choice(CM, P, 1, T), cm_choice(CM, P, 2, T2), T = night(N, R, 2), T2 = night(N, R, 3).

% Other nights target 1: separate from night 1 (uses other_night_role_order, requires alive checks)
{ cm_choice(CM, P, 1, night(N, RoleOrd, 2)) : player(P), P != CM, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)).

% Other nights target 2: separate slot for the second chosen player
{ cm_choice(CM, P, 2, night(N, RoleOrd, 3)) : player(P), P != CM, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)).

% Other nights: two targets must be different players (separate from night 1: different N condition)
:- cm_choice(CM, P, 1, T), cm_choice(CM, P, 2, T2), T = night(N, R, 2), T2 = night(N, R, 3), N > 1.

% Bridge: both target slots generate player_chooses atoms (separate rules per slot for different times)
player_chooses(chambermaid, CM, point(P), T) :- cm_choice(CM, P, 1, T).
player_chooses(chambermaid, CM, point(P), T) :- cm_choice(CM, P, 2, T).

% A player "woke due to their ability" if:
% 1. They have a role with a night action
% 2. They were functioning (not drunk/poisoned)
% 3. They actually used their ability (had a substep action)

% Helper: a player woke if they have a waking role and were functioning at wake time
woke_due_to_ability(P, N) :-
    night_number(N),
    assigned(0, P, R),
    role_wakes_night(R, N),
    player_was_functioning_when_woke(P, R, N).

% Helper: a role wakes on night 1 if it has any first-night substep defined
role_wakes_night(R, 1) :- first_night_substep(R, _, _).
% Helper: a role wakes on other nights if it has any other-night substep (separate: different substep source)
role_wakes_night(R, N) :- other_night_substep(R, _, _), night_number(N), N > 1.

% Night 1 functioning check: uses first_night_role_order (can't merge with other-nights)
player_was_functioning_when_woke(P, R, 1) :-
    assigned(0, P, R),
    first_night_role_order(R, RoleOrd),
    first_night_substep(R, 1, _),
    functioning(P, night(1, RoleOrd, 1)).

% Other nights functioning check: uses other_night_role_order and adds alive check
player_was_functioning_when_woke(P, R, N) :-
    night_number(N), N > 1,
    assigned(0, P, R),
    other_night_role_order(R, RoleOrd),
    other_night_substep(R, 1, _),
    alive(P, night(N, 0, 0)),
    functioning(P, night(N, RoleOrd, 1)).

% Helper: flatten both target slots into one predicate (separate rules per slot because times differ)
cm_target(CM, P, N) :- cm_choice(CM, P, 1, night(N, _, _)).
cm_target(CM, P, N) :- cm_choice(CM, P, 2, night(N, _, _)).

% Other nights: count how many chosen players woke; separate from night 1 (#count can't span both)
cm_woke_count(CM, N, Count) :-
    assigned(0, CM, chambermaid),
    night_number(N), N > 1,
    Count = #count { P : cm_target(CM, P, N), woke_due_to_ability(P, N) }.

% Night 1: count how many chosen players woke (separate: different night number constraint)
cm_woke_count(CM, 1, Count) :-
    assigned(0, CM, chambermaid),
    Count = #count { P : cm_target(CM, P, 1), woke_due_to_ability(P, 1) }.

% Night 1 functioning tell: deterministic count from cm_woke_count (can't merge with other nights: different role order)
st_tells_core(chambermaid, CM, count(Count), night(1, RoleOrd, 4)) :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    functioning(CM, night(1, RoleOrd, 4)),
    cm_woke_count(CM, 1, Count).

% Other nights functioning tell: deterministic count; separate from night 1 (different role order + alive check)
st_tells_core(chambermaid, CM, count(Count), night(N, RoleOrd, 4)) :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)),
    functioning(CM, night(N, RoleOrd, 4)),
    cm_woke_count(CM, N, Count).

% Night 1 not-functioning: ST chooses any count 0-2 (choice rule; can't merge with functioning: different semantics)
{ st_tells_core(chambermaid, CM, count(C), night(1, RoleOrd, 4)) : C = 0..2 } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    not functioning(CM, night(1, RoleOrd, 4)).

% Other nights not-functioning: ST chooses any count 0-2 (separate from night 1: different role order + alive check)
{ st_tells_core(chambermaid, CM, count(C), night(N, RoleOrd, 4)) : C = 0..2 } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)),
    not functioning(CM, night(N, RoleOrd, 4)).

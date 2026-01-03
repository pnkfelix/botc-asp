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

% First night: Chambermaid chooses 2 different alive players (not self)
{ cm_choice(CM, P, 1, night(1, RoleOrd, 2)) : player(P), P != CM } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid).

{ cm_choice(CM, P, 2, night(1, RoleOrd, 3)) : player(P), P != CM } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid).

% Choices must be different players
:- cm_choice(CM, P, 1, T), cm_choice(CM, P, 2, T2), T = night(N, R, 2), T2 = night(N, R, 3).

% Other nights
{ cm_choice(CM, P, 1, night(N, RoleOrd, 2)) : player(P), P != CM, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)).

{ cm_choice(CM, P, 2, night(N, RoleOrd, 3)) : player(P), P != CM, alive(P, night(N, 0, 0)) } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)).

:- cm_choice(CM, P, 1, T), cm_choice(CM, P, 2, T2), T = night(N, R, 2), T2 = night(N, R, 3), N > 1.

% Connect to player_chooses
player_chooses(chambermaid, CM, point(P), T) :- cm_choice(CM, P, 1, T).
player_chooses(chambermaid, CM, point(P), T) :- cm_choice(CM, P, 2, T).

% A player "woke due to their ability" if:
% 1. They have a role with a night action
% 2. They were functioning (not drunk/poisoned)
% 3. They actually used their ability (had a substep action)

% Track if a player woke due to their ability on a given night
woke_due_to_ability(P, N) :-
    night_number(N),
    assigned(0, P, R),
    role_wakes_night(R, N),
    player_was_functioning_when_woke(P, R, N).

% Roles that wake on first night
role_wakes_night(R, 1) :- first_night_substep(R, _, _).
% Roles that wake on other nights (and player is alive)
role_wakes_night(R, N) :- other_night_substep(R, _, _), night_number(N), N > 1.

% Player was functioning when their role woke
player_was_functioning_when_woke(P, R, 1) :-
    assigned(0, P, R),
    first_night_role_order(R, RoleOrd),
    first_night_substep(R, 1, _),
    functioning(P, night(1, RoleOrd, 1)).

player_was_functioning_when_woke(P, R, N) :-
    night_number(N), N > 1,
    assigned(0, P, R),
    other_night_role_order(R, RoleOrd),
    other_night_substep(R, 1, _),
    alive(P, night(N, 0, 0)),
    functioning(P, night(N, RoleOrd, 1)).

% Helper: P is one of the chambermaid's chosen targets for night N
cm_target(CM, P, N) :- cm_choice(CM, P, 1, night(N, _, _)).
cm_target(CM, P, N) :- cm_choice(CM, P, 2, night(N, _, _)).

% Count how many of the chosen players woke
cm_woke_count(CM, N, Count) :-
    assigned(0, CM, chambermaid),
    night_number(N), N > 1,
    Count = #count { P : cm_target(CM, P, N), woke_due_to_ability(P, N) }.

cm_woke_count(CM, 1, Count) :-
    assigned(0, CM, chambermaid),
    Count = #count { P : cm_target(CM, P, 1), woke_due_to_ability(P, 1) }.

% ST tells Chambermaid the count (if functioning)
st_tells_core(chambermaid, CM, count(Count), night(1, RoleOrd, 4)) :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    functioning(CM, night(1, RoleOrd, 4)),
    cm_woke_count(CM, 1, Count).

st_tells_core(chambermaid, CM, count(Count), night(N, RoleOrd, 4)) :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)),
    functioning(CM, night(N, RoleOrd, 4)),
    cm_woke_count(CM, N, Count).

% If not functioning, ST can give any count 0-2
{ st_tells_core(chambermaid, CM, count(C), night(1, RoleOrd, 4)) : C = 0..2 } = 1 :-
    first_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    not functioning(CM, night(1, RoleOrd, 4)).

{ st_tells_core(chambermaid, CM, count(C), night(N, RoleOrd, 4)) : C = 0..2 } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(chambermaid, RoleOrd),
    assigned(0, CM, chambermaid),
    alive(CM, night(N, 0, 0)),
    not functioning(CM, night(N, RoleOrd, 4)).

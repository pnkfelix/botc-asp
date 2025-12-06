% Time points for the game
% We use structured time: night(N, RoleOrder, Substep)
% night(1, 0, 0) is the setup state before any role acts
% night(1, R, S) is substep S of the role at position R in the night order

time(night(1, 0, 0)).  % setup state before any actions on night 1
time(night(1, RoleOrd, S)) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    assigned(0, _, Role).

% Other nights (2+) use other_night_role_order and other_night_substep
% night_number/1 defines which nights we're modeling
night_number(1).  % always model night 1

time(night(N, 0, 0)) :- night_number(N), N > 1.  % setup state for other nights
time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, _, Role).

% Which role is acting at a given time point
acting_role(night(1, RoleOrd, S), Role) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    assigned(0, _, Role).

acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, _, Role).

% Ordering: next(T1, T2) means T2 immediately follows T1
% Ordering is just numeric comparison on (RoleOrder, Substep) within same night
next(T1, T2) :-
    time(T1), time(T2),
    T1 = night(N, R1, S1),
    T2 = night(N, R2, S2),
    (R1, S1) < (R2, S2),
    not between(T1, T2).

% between(T1, T2) is true if there's some T3 strictly between T1 and T2
between(T1, T2) :-
    time(T1), time(T2), time(T3),
    T1 = night(N, R1, S1),
    T2 = night(N, R2, S2),
    T3 = night(N, R3, S3),
    (R1, S1) < (R3, S3),
    (R3, S3) < (R2, S2).

player(P) :- name(P), chair(P,C), C < player_count.

% Circular seating adjacency
% Two players are neighbors if their chairs are adjacent (mod player_count)
neighbor(P1, P2) :-
    player(P1), player(P2),
    chair(P1, C1), chair(P2, C2),
    C2 = (C1 + 1) \ player_count.

% neighbor is symmetric
neighbor(P1, P2) :- neighbor(P2, P1).

% Reminder token inertia: once placed, tokens persist to the next time point
reminder_on(Token, P, T2) :- reminder_on(Token, P, T1), next(T1, T2).

% A player is impaired at time T if they have a token that causes impairment
impaired(P, T) :- reminder_on(Token, P, T), causes_impairment(Token).

% A player's ability is "functioning" if they're not impaired
functioning(P, T) :- player(P), time(T), not impaired(P, T).

% ===========================================================================
% Night communication vocabulary
% These are the signals available for ST <-> player communication at night
% ===========================================================================

% Binary signals
signal(yes).
signal(no).

% Alignment signals
signal(good).
signal(evil).

% Numeric counts (0 through max possible)
signal(count(N)) :- N = 0..player_count.

% Pointing to a player
signal(point(P)) :- player(P).

% Pointing to a role on the script
signal(point(R)) :- role(R).

% st_tells(Role, Player, Message, T) - ST communicates Message to Player
% as part of Role's ability at time T

% player_chooses(Role, Player, Choice, T) - Player communicates Choice to ST
% as part of Role's ability at time T (in response to ST's request)

% Categories for registration (role type and alignment)
category(townsfolk).
category(outsider).
category(minion).
category(demon).
category(good).
category(evil).

% Actual categories of a player based on their assigned role
actual_category(P, townsfolk) :- assigned(0, P, R), townsfolk(R).
actual_category(P, outsider) :- assigned(0, P, R), outsider(R).
actual_category(P, minion) :- assigned(0, P, R), minion(R).
actual_category(P, demon) :- assigned(0, P, R), demon(R).
actual_category(P, good) :- assigned(0, P, R), townsfolk(R).
actual_category(P, good) :- assigned(0, P, R), outsider(R).
actual_category(P, evil) :- assigned(0, P, R), minion(R).
actual_category(P, evil) :- assigned(0, P, R), demon(R).

% Default: register as what you actually are (for players who can't mis-register)
registers_as(P, C, T) :- actual_category(P, C), time(T), not may_mis_register(P).

% If a player may mis-register, ST chooses for each category they may misregister as
{ registers_as(P, C, T) } :- may_misregister_as(P, C), time(T).

% Players who can mis-register can also choose to register as their actual category
% (this is not misregistration, just being truthful)
{ registers_as(P, C, T) } :- actual_category(P, C), time(T), may_mis_register(P).

% Players who can mis-register must still register as at least one role category
:- player(P), time(T), may_mis_register(P),
   not registers_as(P, townsfolk, T), not registers_as(P, outsider, T),
   not registers_as(P, minion, T), not registers_as(P, demon, T).

% Players who can mis-register must still register as at least one alignment
:- player(P), time(T), may_mis_register(P),
   not registers_as(P, good, T), not registers_as(P, evil, T).

% outsider_adjustment(A) :- A = #sum { N, R: causes_outsider_mod(R, N), distrib(R) }.
outsider_adjustment(A) :- A = #sum { N, R: causes_outsider_mod(R, N), assigned(0, _, R) }.

minion_adjustment(A) :- A = #sum { N, R: causes_minion_mod(R, N), assigned(0, _, R) }.
demon_adjustment(A) :- A = #sum { N, R: causes_demon_mod(R, N), assigned(0, _, R) }.

causes_townsfolk_mod(R, -N) :- causes_outsider_mod(R, N).

causes_minion_mod(none, 0) :- #false.
causes_demon_mod(none, 0) :- #false.

townsfolk_adjustment(A) :- A = #sum { N, R: causes_townsfolk_mod(R, N), assigned(0, _, R) }.

adjusted_townsfolk(B + A) :- base_townsfolk(B), townsfolk_adjustment(A).
adjusted_outsider(B + A) :- base_outsider(B), outsider_adjustment(A).
adjusted_minion(B + A) :- base_minion(B), minion_adjustment(A).
adjusted_demon(B + A) :- base_demon(B), demon_adjustment(A).

role(X) :- townsfolk(X).
role(X) :- outsider(X).
role(X) :- minion(X).
role(X) :- demon(X).

reminder(poi_poisoned).

% the legal baseline role distrbutions are:
% 7 players => 1 demon, 1 minion, 0 outsiders, 5 townsfolk

base_townsfolk(5) :- 7 <= player_count <= 9.
base_townsfolk(7) :- 10 <= player_count <= 12.
base_townsfolk(9) :- 13 <= player_count <= 15.

base_outsider(player_count - 7) :- 7 <= player_count <= 9.
base_outsider(player_count - 10) :- 10 <= player_count <= 12.
base_outsider(player_count - 13) :- 13 <= player_count <= 15.

base_minion(1) :-  7 <= player_count <= 9.
base_minion(2) :- 10 <= player_count <= 12.
base_minion(3) :- 13 <= player_count <= 15.

base_demon(1).

distrib(R) :- assigned(0, _, R).
%{distrib(X) : demon(X)} = 1.
%{distrib(X) : minion(X)} = 1    :- 7 <= player_count <= 9.
%{distrib(X) : townsfolk(X)} = 5 - A :- 7 <= player_count <= 9, outsider_adjustment(A).
%{distrib(X) : outsider(X)} = player_count - 7 + A :- 7 <= player_count <= 9, outsider_adjustment(A).
%
%{distrib(X) : minion(X)} = 2    :- 10 <= player_count <= 12.
%{distrib(X) : townsfolk(X)} = 7 - A :- 10 <= player_count <= 12, outsider_adjustment(A).
%{distrib(X) : outsider(X)} = player_count - 10 + A :- 10 <= player_count <= 12, outsider_adjustment(A).
%
%{distrib(X) : minion(X)} = 3    :- 13 <= player_count <= 15.
%{distrib(X) : townsfolk(X)} = 9 :- 13 <= player_count <= 15.
%{distrib(X) : outsider(X)} = player_count - 13 + A :- 13 <= player_count <= 15, outsider_adjustment(A).

% belt+suspenders: the number of distributed roles must equal player count
%:- not {distrib(X)} = player_count.

% most roles can be in the bag (except things like drunk, marionette).
{bag(X) : role(X), not never_in_bag(X)} = player_count.

% each player receives exactly one token from the bag.
{received(P, X) : bag(X)} = 1 :- player(P).
% a token is not received by multiple players
:- received(P1, X), received(P2, X), P1 != P2.

% each player is assigned exactly one unique role
{ assigned(0, P, R) : role(R) } = 1 :- player(P).
R1 != R2 :- assigned(0, P1, R1), assigned(0, P2, R2), P1 != P2.

%{assigned(0, P, X) : player(P), distrib(X)} = player_count.
%R1 = R2 :- assigned(0, P, R1), assigned(0, P, R2).
%R1 != R2 :- assigned(0, P1, R1), assigned(0, P2, R2), P1 != P2.

thinks_it_is(X, X) :- role(X), not mistaken_identity(X, _).
thinks_it_is(R, Y) :- mistaken_identity(R, townsfolk), townsfolk(Y).
thinks_it_is(R, Y) :- mistaken_identity(R, demon), demon(Y).
thinks_it_is(R, Y) :- mistaken_identity(R, minion), minion(Y).

% the role assigned is one that "thinks it is the" token received.
:- received(P, T), assigned(_, P, R), not thinks_it_is(R, T).

:- adjusted_townsfolk(N), #count { R : assigned(0, _, R), townsfolk(R) } != N.
:- adjusted_outsider(N), #count { R : assigned(0, _, R), outsider(R) } != N.
:- adjusted_minion(N), #count { R : assigned(0, _, R), minion(R) } != N.
:- adjusted_demon(N), #count { R : assigned(0, _, R), demon(R) } != N.

#include "types.lp".

% Time points for the game
% We use structured time: night(N, RoleOrder, Substep)
% night(1, 0, 0) is the setup state before any role acts
% night(1, R, S) is substep S of the role at position R in the night order

time(night(1, 0, 0)).  % setup state before any actions on night 1

% Generate time for roles that are actually assigned
time(night(1, RoleOrd, S)) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    assigned(0, _, Role).

% Also generate time for roles that players THINK they are (e.g., Drunk thinks they're Empath)
% This is needed because the ST still "wakes" them and gives them (fake) info
time(night(1, RoleOrd, S)) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    received(_, Role).

% Other nights (2+) use other_night_role_order and other_night_substep
% night_number/1 defines which nights we're modeling
% Tests/instances can add needs_night(N) to request modeling of night N;
% all nights from 1 to N will be included automatically.
night_number(1).  % always model night 1
night_number(N) :- needs_night(N).  % include any explicitly requested night
night_number(N) :- night_number(N+1), N >= 1.  % fill in all nights below

time(night(N, 0, 0)) :- night_number(N), N > 1.  % setup state for other nights

time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, _, Role).

time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    received(_, Role).

% Which role is acting at a given time point
% Includes both assigned roles and roles players think they are (received tokens)
acting_role(night(1, RoleOrd, S), Role) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    assigned(0, _, Role).

acting_role(night(1, RoleOrd, S), Role) :-
    first_night_role_order(Role, RoleOrd),
    first_night_substep(Role, S, _),
    received(_, Role).

acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, _, Role).

acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    received(_, Role).

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

% Phase transitions: night -> day -> night
% The final time of night N transitions to day(N, 0)
next(T, day(N, 0)) :- final_night_time(N, T), day_number(N).

% Within a day: day(N, 0) -> day(N, exec)
next(day(N, 0), day(N, exec)) :- day_number(N).

% Day N ends, night N+1 begins
next(day(N, exec), night(N+1, 0, 0)) :- day_number(N), night_number(N+1).

player(P) :- name(P), chair(P,C), C < player_count.

% Circular seating adjacency
% Two players are neighbors if their chairs are adjacent (mod player_count)
neighbor(P1, P2) :-
    player(P1), player(P2),
    chair(P1, C1), chair(P2, C2),
    C2 = (C1 + 1) \ player_count.

% neighbor is symmetric
neighbor(P1, P2) :- neighbor(P2, P1).

% Order two players by chair number (smaller chair first)
% Used to normalize player order in info tuples so position doesn't leak info
chair_ordered(P1, P2, P1, P2) :- player(P1), player(P2), chair(P1, C1), chair(P2, C2), C1 < C2.
chair_ordered(P1, P2, P2, P1) :- player(P1), player(P2), chair(P1, C1), chair(P2, C2), C1 > C2.

% ===========================================================================
% Alive vs Publicly Alive
% ===========================================================================
% alive(P, T) = true dead/alive state (what ST knows, used for game mechanics)
% publicly_alive(P, T) = what players see (death shroud not yet placed)
%
% During the night, a player killed by the demon has imp_dead token.
% They are dead (for ability purposes) but publicly_alive (shroud placed at dawn).

% A player is dead (from ST perspective) if they have a "dead tonight" token
% Note: imp_dead specifically is handled in tb.lp to account for protection
% Other death tokens could be added here as needed
% dead(P, T) :- reminder_on(some_other_death_token, P, T).

% publicly_alive = alive and not yet revealed as dead
% (The public death shroud is placed at dawn, so during night they look alive)
publicly_alive(P, T) :- alive(P, T).

% ===========================================================================
% Living neighbors: the closest alive player in each direction
% Used by Empath and potentially other roles
% These use `alive` (ST perspective) not `publicly_alive`
% ===========================================================================

% clockwise_distance(P1, P2, D) = going clockwise from P1, P2 is D seats away
clockwise_distance(P1, P2, D) :-
    player(P1), player(P2), P1 != P2,
    chair(P1, C1), chair(P2, C2),
    D = (C2 - C1 + player_count) \ player_count.

% living_neighbor(P1, P2, T) means P2 is a living neighbor of P1 at time T
% P2 is a living neighbor if they're alive (not dead) and no one closer is alive

% Clockwise living neighbor: closest alive player going clockwise
living_neighbor_clockwise(P1, P2, T) :-
    player(P1), player(P2), P1 != P2,
    time(T),
    alive(P2, T),
    not dead(P2, T),
    clockwise_distance(P1, P2, D),
    not closer_alive_clockwise(P1, P2, D, T).

% There's a closer alive (and not dead) player clockwise
closer_alive_clockwise(P1, P2, D, T) :-
    player(P1), player(P2), player(P3),
    P1 != P2, P1 != P3, P2 != P3,
    time(T),
    alive(P3, T),
    not dead(P3, T),
    clockwise_distance(P1, P2, D),
    clockwise_distance(P1, P3, D3),
    D3 < D.

% Counter-clockwise living neighbor: closest alive player going counter-clockwise
% This is equivalent to clockwise with reversed direction
living_neighbor_counterclockwise(P1, P2, T) :-
    living_neighbor_clockwise(P2, P1, T).

% A player's living neighbors are the closest alive player in each direction
living_neighbor(P1, P2, T) :- living_neighbor_clockwise(P1, P2, T).
living_neighbor(P1, P2, T) :- living_neighbor_counterclockwise(P1, P2, T).

% Reminder token inertia: once placed, tokens persist to the next time point
% (unless explicitly removed via token_removed)
reminder_on(Token, P, T2) :- reminder_on(Token, P, T1), next(T1, T2), not token_removed(Token, P, T2).

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

% st_tells_core(Role, Player, Message, T) - ST communicates Message to Player
% as part of Role's ability at time T. This is the detailed/implementation form.

% Constraint: st_tells_core for a role can only go to players who think they have
% that role (i.e., received that role's token). This covers both:
% 1. Players who actually have the role
% 2. Players who think they have it (e.g., Drunk with chef token)
% NOTE: This will need adjustment for Philosopher (gains another role's ability),
% Pixie (gains ability after townsfolk dies), and Pit Hag (can change roles mid-game).
:- st_tells_core(R, P, _, _), not received(P, R).

% Constraint: a player can only receive one message from a role at a given time.
% Roles that communicate multiple pieces of info (e.g., Washerwoman pointing to
% two players and naming a role) use tuple values like info(P1, P2, Role).
% Using aggregate instead of pairwise comparison to avoid grounding explosion.
:- st_tells_core(R, P, _, T), #count { V : st_tells_core(R, P, V, T) } > 1.

% ===========================================================================
% Simplified st_tells predicates for player-visible information
% These drop implementation details (player identity, exact timing) and expose
% what players actually observe in a simplified form.
% ===========================================================================

% --- Binary st_tells/2: for one-time info or when night doesn't matter ---

% For count info (chef, empath): st_tells(chef, 2) instead of st_tells(chef, count(2))
st_tells(R, N) :- st_tells_core(R, _, count(N), _).
st_tells(P, N) :- st_tells_core(_, P, count(N), _).

% For info tuples (washerwoman, librarian, investigator): extract the role shown
% st_tells(washerwoman, chef) means "washerwoman was told about a chef"
st_tells(R, Role) :- st_tells_core(R, _, info(_, _, Role), _).
st_tells(P, Role) :- st_tells_core(_, P, info(_, _, Role), _).

% For point info (undertaker, ravenkeeper): st_tells(undertaker, poisoner)
st_tells(R, Role) :- st_tells_core(R, _, point(Role), _).
st_tells(P, Role) :- st_tells_core(_, P, point(Role), _).

% For yes/no (fortune_teller): already simple, pass through directly
st_tells(R, yes) :- st_tells_core(R, _, yes, _).
st_tells(R, no) :- st_tells_core(R, _, no, _).
st_tells(P, yes) :- st_tells_core(_, P, yes, _).
st_tells(P, no) :- st_tells_core(_, P, no, _).

% --- Ternary st_tells/3: includes night number for recurring info roles ---
% Use this for empath, fortune_teller, or any role that gets info multiple nights.
% Format: st_tells(RoleOrPlayer, Info, NightNum)

% For count info with night: st_tells(empath, 1, 2) = "empath told 1 on night 2"
st_tells(R, N, Night) :- st_tells_core(R, _, count(N), night(Night, _, _)).
st_tells(P, N, Night) :- st_tells_core(_, P, count(N), night(Night, _, _)).

% For yes/no with night: st_tells(fortune_teller, yes, 3) = "FT told yes on night 3"
st_tells(R, yes, Night) :- st_tells_core(R, _, yes, night(Night, _, _)).
st_tells(R, no, Night) :- st_tells_core(R, _, no, night(Night, _, _)).
st_tells(P, yes, Night) :- st_tells_core(_, P, yes, night(Night, _, _)).
st_tells(P, no, Night) :- st_tells_core(_, P, no, night(Night, _, _)).

% For point info with night (undertaker gets info on night after execution)
st_tells(R, Role, Night) :- st_tells_core(R, _, point(Role), night(Night, _, _)).
st_tells(P, Role, Night) :- st_tells_core(_, P, point(Role), night(Night, _, _)).

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

% Mid-game role changes: register based on currently assigned role
% If assigned a demon role at day N, register as demon for all times after day N
registers_as(P, demon, T) :-
    assigned(N, P, R), demon(R), N > 0,
    time(T), T = night(N2, _, _), N2 > N.
registers_as(P, demon, T) :-
    assigned(N, P, R), demon(R), N > 0,
    time(T), T = day(N2, _), N2 > N.

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
% NOTE: Only checks initial assignment (time 0). Mid-game role changes like
% Scarlet Woman becoming Imp are not constrained by this. If we need to track
% current role at arbitrary times, we may need a current_role(P, R, T) fluent.
:- received(P, T), assigned(0, P, R), not thinks_it_is(R, T).

:- adjusted_townsfolk(N), #count { R : assigned(0, _, R), townsfolk(R) } != N.
:- adjusted_outsider(N), #count { R : assigned(0, _, R), outsider(R) } != N.
:- adjusted_minion(N), #count { R : assigned(0, _, R), minion(R) } != N.
:- adjusted_demon(N), #count { R : assigned(0, _, R), demon(R) } != N.

% ===========================================================================
% Day Phase - Simplified
% We only track: reminder tokens, executions, and deaths
% Nominations and voting details are abstracted away
% ===========================================================================

% day_number/1 defines which days we're modeling (like night_number)
% Each day N follows night N, so we model the same range as nights
day_number(N) :- night_number(N).

% Everyone is alive at the start of the game
alive(P, night(1, 0, 0)) :- player(P).

% Alive status persists through night unless killed or dead
alive(P, T2) :- alive(P, T1), next(T1, T2), not died(P, T1), not dead(P, T2).

% Alive at start of day = alive at end of preceding night (unless they died that night)
% Note: dead(P, T) handles imp_dead token deaths during the night
alive(P, day(N, 0)) :- alive(P, T), final_night_time(N, T), not died(P, T), not dead(P, T).

% Helper: find the final time point of a night
final_night_time(N, T) :-
    time(T), T = night(N, _, _),
    not next(T, T2) : time(T2), T2 = night(N, _, _).

% Alive at start of night N+1 = alive at end of day N
alive(P, night(N+1, 0, 0)) :- alive(P, day(N, exec)), night_number(N+1).

% ===========================================================================
% Day Time Structure (simplified)
% day(N, Phase) where Phase is:
%   0 = start of day
%   exec = end of day (execution happens here)
% ===========================================================================

day_time(day(N, 0)) :- day_number(N).
day_time(day(N, exec)) :- day_number(N).

% Time includes day times
time(T) :- day_time(T).

% Alive at end of day = alive at start, minus anyone who died during day
alive(P, day(N, exec)) :- alive(P, day(N, 0)), not died_during_day(P, N).

% Helper: died during day N
died_during_day(P, N) :- died(P, day(N, _)).

% ===========================================================================
% Execution and Death
% executed(Player, Day) - INPUT: who was executed on this day
% died(Player, Time) - derived from execution or other effects
% ===========================================================================

% A player dies when executed
died(P, day(N, exec)) :- executed(P, N).

% Validate: executed player must have been alive at start of day
:- executed(P, N), not alive(P, day(N, 0)).

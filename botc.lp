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

% Only generate time points for roles whose player is alive at start of night
% This prevents dead players' roles from appearing in the timeline
time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, Player, Role),
    alive(Player, night(N, 0, 0)).

time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    received(Player, Role),
    alive(Player, night(N, 0, 0)).

% Also generate time points for players who CURRENTLY have a role (via role change)
% This handles cases like Scarlet Woman becoming Imp after original Imp is executed
time(night(N, RoleOrd, S)) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    character_assignment_state_at_time(night(N, 0, 0), Player, Role),
    alive(Player, night(N, 0, 0)).

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

% Only generate acting_role for roles whose player is alive at start of night
% This prevents dead players from "waking" in subsequent nights
acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    assigned(0, Player, Role),
    alive(Player, night(N, 0, 0)).

acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    received(Player, Role),
    alive(Player, night(N, 0, 0)).

% Also generate acting_role for players who CURRENTLY have a role (via role change)
acting_role(night(N, RoleOrd, S), Role) :-
    night_number(N), N > 1,
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    character_assignment_state_at_time(night(N, 0, 0), Player, Role),
    alive(Player, night(N, 0, 0)).

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

% Phase transitions: night -> dawn -> day -> night
% Dawn is when deaths from the night are announced publicly
% The final time of night N transitions to dawn(N)
next(T, dawn(N)) :- final_night_time(N, T), day_number(N).

% dawn(N) is a time point
time(dawn(N)) :- day_number(N).

% Dawn transitions to day start
next(dawn(N), day(N, 0)) :- day_number(N), not game_over(dawn(N)).

% Within a day: day(N, 0) -> day(N, exec)
% Don't transition if evil already won at start of day
next(day(N, 0), day(N, exec)) :- day_number(N), not game_over(day(N, 0)).

% Day N ends, night N+1 begins
% Don't transition to next night if game ended (good wins from execution, or evil wins)
next(day(N, exec), night(N+1, 0, 0)) :- day_number(N), night_number(N+1), not game_over(day(N, exec)).

player(P) :- name(P), chair(P,C), C < player_count.

% Game chair: only chairs for players actually in this game
game_chair(P, C) :- player(P), chair(P, C).

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
% Alive/Dead State: ST Perspective vs Public Knowledge
% ===========================================================================
% There are two parallel tracks for death status:
%
% ST PERSPECTIVE (game mechanics):
%   alive(P, T) = player is alive at time T (for ability purposes)
%   dead_at_time(P, T) = player is dead at time T (for ability purposes)
%
% PUBLIC KNOWLEDGE (what players see):
%   publicly_alive(P, T) = death shroud not yet placed
%   publicly_dead_at_time(P, T) = death has been announced, shroud placed
%
% During the night, a player killed by the demon has a death token.
% They are dead_at_time (for ability purposes) but not publicly_dead_at_time
% until dawn when the shroud is placed and death is announced.
%
% Example: Imp kills Alice at night(2, 5, 3)
%   - dead_at_time(alice, night(2, 5, 3)) is TRUE (she's dead for mechanics)
%   - publicly_dead_at_time(alice, night(2, 5, 3)) is FALSE (not announced yet)
%   - publicly_dead_at_time(alice, dawn(2)) is TRUE (announced at dawn)

% A player is dead (from ST perspective) if they have a "dead tonight" token
% Note: imp_dead specifically is handled in tb.lp to account for protection
% Other death tokens could be added here as needed
% dead_at_time(P, T) :- reminder_on(some_other_death_token, P, T).

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
    not dead_at_time(P2, T),
    clockwise_distance(P1, P2, D),
    not closer_alive_clockwise(P1, P2, D, T).

% There's a closer alive (and not dead) player clockwise
closer_alive_clockwise(P1, P2, D, T) :-
    player(P1), player(P2), player(P3),
    P1 != P2, P1 != P3, P2 != P3,
    time(T),
    alive(P3, T),
    not dead_at_time(P3, T),
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
% Generic Demon Kill Logic
% Each demon places its own death token via reminder_on rules.
% The kill processing (success check, death recording) is shared here.
% ===========================================================================

% Demons register their kill tokens here
% (Each demon's .lp file adds: demon_kill_token(X_dead).)

% Generic: kill succeeds if death token is on player and not protected
% GROUNDING OPTIMIZATION: Start with small domains (final_night_time, player, demon_kill_token)
% then look up reminder_on. This avoids iterating through all reminder_on atoms.
demon_kill_succeeds(P, T) :-
    final_night_time(N, T),
    player(P),
    demon_kill_token(Token),
    reminder_on(Token, P, T),
    not protected_from_demon(P, T),
    not demon_kill_blocked(Token, P, T).

% Hook for special cases (e.g., Fang Gu jump - target becomes demon, not dead)
% Demons can add demon_kill_blocked(Token, P, T) to prevent the kill.

% Player becomes dead at the time the kill succeeds
dead_at_time(P, T) :- demon_kill_succeeds(P, T).

% Death is recorded at the final time of the night
died(P, T) :-
    final_night_time(N, T),
    demon_kill_succeeds(P, T),
    alive(P, night(N, 0, 0)).

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
% The Role identifies which ability triggered the communication.
% The Player is whoever receives the message (usually the role holder, but not always -
% e.g., Cerenovus tells the target what role to be mad as).

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

% Also extract the players pointed to from info tuples
% st_tells(washerwoman, alice) means "washerwoman pointed to player alice"
st_tells(R, P1) :- st_tells_core(R, _, info(P1, _, _), _).
st_tells(R, P2) :- st_tells_core(R, _, info(_, P2, _), _).
st_tells(P, P1) :- st_tells_core(_, P, info(P1, _, _), _).
st_tells(P, P2) :- st_tells_core(_, P, info(_, P2, _), _).

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
% Use the fine-grained predicate to track current role at each time point
registers_as(P, demon, T) :-
    character_assignment_state_at_time(T, P, R),
    demon(R),
    not assigned(0, P, R).  % Only for mid-game changes (not initial demon)

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
% 5 players => 1 demon, 1 minion, 0 outsiders, 3 townsfolk
% 6 players => 1 demon, 1 minion, 1 outsider, 3 townsfolk
% 7 players => 1 demon, 1 minion, 0 outsiders, 5 townsfolk

base_townsfolk(3) :- 5 <= player_count <= 6.
base_townsfolk(5) :- 7 <= player_count <= 9.
base_townsfolk(7) :- 10 <= player_count <= 12.
base_townsfolk(9) :- 13 <= player_count <= 15.

base_outsider(player_count - 5) :- 5 <= player_count <= 6.
base_outsider(player_count - 7) :- 7 <= player_count <= 9.
base_outsider(player_count - 10) :- 10 <= player_count <= 12.
base_outsider(player_count - 13) :- 13 <= player_count <= 15.

base_minion(1) :-  5 <= player_count <= 9.
base_minion(2) :- 10 <= player_count <= 12.
base_minion(3) :- 13 <= player_count <= 15.

base_demon(1).

distrib(R) :- assigned(0, _, R).

% Assertion predicates: allow instances to require certain predicates hold
% assert_distrib(R) requires that role R must be distributed (assigned to some player)
:- assert_distrib(R), not distrib(R).
% assert_assigned(N, P, R) requires that player P is assigned role R
%   - assert_assigned(0, P, R): initial assignment (P starts as role R)
%   - assert_assigned(N, P, R) for N >= 1: at start of night N, P has role R
:- assert_assigned(N, P, R), not assigned(N, P, R).
% assert_reminder_on(Token, P, T) requires that reminder Token is on player P at time T
:- assert_reminder_on(Token, P, T), not reminder_on(Token, P, T).
% assert_received(P, R) requires that player P received token R
:- assert_received(P, R), not received(P, R).
% assert_drawn(R) requires that role R is in the bag (was drawn from script into bag)
:- assert_drawn(R), not bag(R).

% Bluffs: 3 good roles shown to the demon that are not in the bag
% A role can be a bluff if it's a good role (townsfolk or outsider) and not in the bag
can_be_bluff(R) :- townsfolk(R), not bag(R).
can_be_bluff(R) :- outsider(R), not bag(R).
% Choose exactly 3 bluffs from available candidates
{bluff(R) : can_be_bluff(R)} = 3.
% assert_bluff(R) requires that role R must be a bluff
:- assert_bluff(R), not bluff(R).

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
% Fine-Grained Role Assignment Tracking
% ===========================================================================
%
% character_assignment_state_at_time(T, P, R) - the internal/fine-grained predicate
% that tracks what role a player has at each specific time point T.
%
% This predicate uses inertia: once a player has a role, they keep it until
% a role change event occurs.
%
% Role changes are recorded via role_assignment_changes_to(T, P, R), which
% indicates that player P's role becomes R at time T.
%
% The coarse-grained assigned(N, P, R) for N >= 1 is derived from this,
% representing the player's role at the start of night N.
%
% assigned(0, P, R) remains the INPUT predicate for initial setup.
% ===========================================================================

% Initialize fine-grained state from initial assignments at night(1, 0, 0)
character_assignment_state_at_time(night(1, 0, 0), P, R) :- assigned(0, P, R).

% Inertia: role persists through time unless a role change occurs at the next time
character_assignment_state_at_time(T2, P, R) :-
    character_assignment_state_at_time(T1, P, R),
    next(T1, T2),
    not role_assignment_changed_at(P, T2).

% Update role when a role change event occurs
character_assignment_state_at_time(T, P, R) :-
    role_assignment_changes_to(T, P, R).

% Helper: track that a role change occurred for a player at a time
role_assignment_changed_at(P, T) :- role_assignment_changes_to(T, P, _).

% Derive coarse-grained assigned(N, P, R) for N >= 1: role at start of night N
assigned(N, P, R) :-
    night_number(N), N >= 1,
    character_assignment_state_at_time(night(N, 0, 0), P, R).

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
alive(P, T2) :- alive(P, T1), next(T1, T2), not died(P, T1), not dead_at_time(P, T2).

% Alive at dawn = alive at end of preceding night (unless they died that night)
% Note: dead_at_time(P, T) handles imp_dead token deaths during the night
% This is when deaths are publicly announced (shroud placed)
alive(P, dawn(N)) :- alive(P, T), final_night_time(N, T), not died(P, T), not dead_at_time(P, T).

% Alive at start of day = alive at dawn (unless something killed them at dawn)
alive(P, day(N, 0)) :- alive(P, dawn(N)), not died(P, dawn(N)), not dead_at_time(P, dawn(N)).

% Helper: find the final time point of a night
final_night_time(N, T) :-
    time(T), T = night(N, _, _),
    not next(T, T2) : time(T2), T2 = night(N, _, _).

% Alive at start of night N+1 = alive at end of day N
% Don't propagate if game ended on day N
alive(P, night(N+1, 0, 0)) :- alive(P, day(N, exec)), night_number(N+1), not game_over(day(N, exec)).

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

% A player is dead at the moment they die (ST perspective - immediate)
dead_at_time(P, T) :- died(P, T).

% Once dead, stay dead (death persists through all subsequent time points)
dead_at_time(P, T2) :- dead_at_time(P, T1), next(T1, T2).

% ===========================================================================
% Public Death Knowledge
% ===========================================================================
% publicly_dead_at_time(P, T) = what players know (death has been announced)
%
% Death announcements happen at specific times:
%   - Execution deaths: announced immediately at day(N, exec)
%   - Night deaths: announced at dawn(N)
%
% Before the announcement, a player might be dead (for game mechanics) but
% not publicly known to be dead (shroud not yet placed).
% ===========================================================================

% A player becomes publicly dead when their death is announced
publicly_dead_at_time(P, T) :- d_death_announced(P, T).

% Once publicly dead, stay publicly dead
publicly_dead_at_time(P, T2) :- publicly_dead_at_time(P, T1), next(T1, T2).

% Validate: executed player must have been alive at start of day
:- executed(P, N), not alive(P, day(N, 0)).

% ===========================================================================
% Execution Tokens
% These tokens mark which player was executed on each day, making it easier
% to visualize and manipulate timeline constraints.
% ===========================================================================

% Execution tokens for each day (not owned by any role - general game tokens)
reminder_label(ex_d1, "Ex D1").
reminder_label(ex_d2, "Ex D2").
reminder_label(ex_d3, "Ex D3").
reminder_label(ex_d4, "Ex D4").
reminder_label(ex_d5, "Ex D5").

% Automatically place execution tokens when a player is executed
reminder_on(ex_d1, P, day(1, exec)) :- executed(P, 1).
reminder_on(ex_d2, P, day(2, exec)) :- executed(P, 2).
reminder_on(ex_d3, P, day(3, exec)) :- executed(P, 3).
reminder_on(ex_d4, P, day(4, exec)) :- executed(P, 4).
reminder_on(ex_d5, P, day(5, exec)) :- executed(P, 5).

% ===========================================================================
% Grimoire State (for rendering)
% These predicates provide a clean interface for a frontend to render the
% grimoire at any point in time with minimal deduction.
% ===========================================================================

% grimoire_token(Player, RoleToken, Time) - what role token to display for a player
% This is the token they hold (what they think they are), not their true role.
% A Drunk with the Chef token displays as Chef.
% After starpass/SW takeover, the new demon displays as Imp (or other demon).
%
% Uses inertia like character_assignment_state_at_time:
% 1. Initial token is from received(P, R) at night(1, 0, 0)
% 2. Token persists through time unless st_tells_role_change occurs
% 3. When player is told their new role, token updates to that role

% Initial grimoire token from what they received
grimoire_token(P, R, night(1, 0, 0)) :- received(P, R).

% Inertia: token persists through time unless a role change notification occurred
grimoire_token(P, R, T2) :-
    grimoire_token(P, R, T1),
    next(T1, T2),
    not grimoire_token_changed_at(P, T2).

% Update token when ST tells player their new role (starpass, Scarlet Woman, etc.)
grimoire_token(P, R, T) :- st_tells_role_change(P, R, T).

% Helper: detect that a token change occurred at this time
grimoire_token_changed_at(P, T) :- st_tells_role_change(P, _, T).

% grimoire_reminder(Player, ReminderToken, Time) - which reminder tokens to show
grimoire_reminder(P, Token, T) :- reminder_on(Token, P, T).

% grimoire_alive(Player, Time) - whether the player is alive
grimoire_alive(P, T) :- alive(P, T).

% grimoire_true_role(Player, Role) - the player's actual role (ST view only)
% This is for the ST's knowledge, not displayed on tokens.
grimoire_true_role(P, R) :- assigned(0, P, R).

% ===========================================================================
% Delta predicates (d_ prefix)
% These are "event" predicates that represent a change/delta in game state.
% The d_ prefix makes them easy to identify for timeline rendering - only d_
% predicates should appear as clickable events in the timeline.
%
% Contrast with state predicates (alive, dead, reminder_on) which represent
% ongoing state that persists across time points.
% ===========================================================================

% d_st_tells/4 - ST communicates info to a player as part of a role's ability
% This is the event of telling, not the state of having been told.
d_st_tells(R, P, M, T) :- st_tells_core(R, P, M, T).

% d_player_chooses/4 - Player makes a choice as part of a role's ability
% This is the event of choosing, not the state of having chosen.
d_player_chooses(R, P, C, T) :- player_chooses(R, P, C, T).

% d_executed/2 - Player is executed
% This is the event of execution happening on a specific day.
d_executed(P, D) :- executed(P, D).

% d_died/2 - Player dies at a specific time
% This captures all death events (execution, imp kill, slayer, etc.)
% The parser computes alive/dead state from this, avoiding O(players × time) output.
d_died(P, T) :- died(P, T).

% d_death_announced/2 - Death is publicly announced
% Night deaths are announced at dawn, execution deaths are announced at execution time.
% This is what appears in the timeline as the "public" death event.
d_death_announced(P, dawn(N)) :- died(P, night(N, _, _)).
d_death_announced(P, day(N, exec)) :- died(P, day(N, exec)).

% ===========================================================================
% Role Change Timeline Events
% ===========================================================================
% When a player's role changes mid-game (e.g., Scarlet Woman becomes demon,
% Imp starpass), the storyteller must tell them about their new role.
%
% Role changes can happen at different phases:
%   - During day (e.g., SW becomes demon when demon is executed)
%   - During night (e.g., starpass when Imp kills itself)
%
% The ST tells the player immediately after the transformation:
%   - Day N changes (e.g., SW at execution) → told at day(N, exec)
%   - Night N changes (e.g., starpass) → told at dawn(N)
%
% Timeline sequence: night(N) → dawn(N) → day(N) → night(N+1) → dawn(N+1)
%
% st_tells_role_change/3 is the event of ST telling the player:
%   st_tells_role_change(Player, ToldRole, Time)
%   where ToldRole is what the player is told (usually their actual new role,
%   but could differ if they're drunk/poisoned/etc.)
%
% Role changes are now tracked via the fine-grained predicate:
%   role_assignment_changes_to(T, P, R) - player P becomes role R at time T
%
% The helper predicates role_changed_at_day and role_changed_at_night
% are derived from role_assignment_changes_to for backward compatibility.
% ===========================================================================

% Role changes during day phase (e.g., SW becomes demon at execution)
% Derived from fine-grained role_assignment_changes_to
role_changed_at_day(P, OldRole, NewRole, N) :-
    role_assignment_changes_to(day(N, exec), P, NewRole),
    assigned(0, P, OldRole),
    OldRole != NewRole.

st_tells_role_change(P, NewRole, day(N, exec)) :-
    role_changed_at_day(P, _, NewRole, N).

% Role changes during night phase (e.g., starpass, barber swap, pit-hag)
% Matches role changes at dawn(N) OR during night(N, _, _)
% Derived from fine-grained role_assignment_changes_to
role_changed_at_night(P, OldRole, NewRole, N) :-
    role_assignment_changes_to(dawn(N), P, NewRole),
    assigned(0, P, OldRole),
    OldRole != NewRole.

role_changed_at_night(P, OldRole, NewRole, N) :-
    role_assignment_changes_to(T, P, NewRole),
    T = night(N, _, _),
    assigned(0, P, OldRole),
    OldRole != NewRole.

% Players are told about role changes immediately when they happen
st_tells_role_change(P, NewRole, T) :-
    role_assignment_changes_to(T, P, NewRole).

% Generic role_changed/4 for convenience (either day or night changes)
role_changed(P, OldRole, NewRole, N) :- role_changed_at_day(P, OldRole, NewRole, N).
role_changed(P, OldRole, NewRole, N) :- role_changed_at_night(P, OldRole, NewRole, N).

% d_st_tells_role_change/3 - Timeline event for role change notification
% This is the event of the ST telling a player about their new role.
d_st_tells_role_change(P, R, T) :- st_tells_role_change(P, R, T).

% d_assigned_change/4 - Player's character assignment changes mid-game
% This is the event of a role transfer (starpass, Scarlet Woman becoming demon, etc.)
% Format: d_assigned_change(Time, Player, OldRole, NewRole)
% Derived directly from role_assignment_changes_to

d_assigned_change(T, Player, OldRole, NewRole) :-
    role_assignment_changes_to(T, Player, NewRole),
    assigned(0, Player, OldRole),
    OldRole != NewRole.

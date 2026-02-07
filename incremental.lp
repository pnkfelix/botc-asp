% incremental.lp - Single-transition validation mode
%
% This file provides the minimal infrastructure needed to validate ONE action
% without deriving the full game history. State is provided as INPUT FACTS
% rather than derived from initial conditions.
%
% USAGE:
%   1. Include this file (NOT botc.lp's inertia rules)
%   2. Include role definitions (tb.lp or individual role files)
%   3. Provide current state as inc_* facts
%   4. Constrain the proposed action
%   5. Check SAT/UNSAT
%
% This allows the SAME role constraint definitions to work for both:
%   - Full trace mode (botc.lp): derives state via inertia from initial setup
%   - Incremental mode (this file): accepts state as input, validates one action

% ===========================================================================
% INPUT PREDICATES (must be provided by caller)
% ===========================================================================
%
% REQUIRED:
%   name(P), chair(P, C)     - Players and seating (standard)
%   #const player_count = N  - Number of players
%   current_night(N)         - Which night we're validating (N >= 2)
%
%   inc_role(P, R)           - Current role assignment for player P
%   inc_alive(P)             - Player P is currently alive
%
% OPTIONAL:
%   inc_initial_role(P, R)   - Initial role assignment (for starpass eligibility)
%                              If not provided, defaults to current role
%   inc_reminder(Token, P)   - Reminder token Token is on player P
%   inc_minion_alive         - At least one minion is alive (for starpass)
%   inc_poisoned(P)          - Player P is poisoned (not functioning)
%
% The caller also constrains the proposed action, e.g.:
%   :- not player_chooses(imp, diana, point(alice), night(2, 4, 2)).

% ===========================================================================
% PLAYER SETUP (same as botc.lp)
% ===========================================================================

player(P) :- name(P), chair(P, C), C < player_count.

% ===========================================================================
% TIME MODEL (minimal - current night + night 1 setup)
% ===========================================================================

% Model the current night
night_number(N) :- current_night(N).

% Night 1 setup time - needed for roles that place reminders at game start
% (e.g., Fortune Teller red herring). We include this so setup rules can fire
% and pick valid values, even though we're validating a later night.
time(night(1, 0, 0)).
night_number(1).

% Setup state at start of current night
time(night(N, 0, 0)) :- current_night(N).

% Generate time points only for roles actually in play
time(night(N, RoleOrd, S)) :-
    current_night(N),
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    inc_role(_, Role).

% Also generate for roles players THINK they have (drunk, etc.)
time(night(N, RoleOrd, S)) :-
    current_night(N),
    other_night_role_order(Role, RoleOrd),
    other_night_substep(Role, S, _),
    inc_received(_, Role).

% Minimal ordering within this night
next(T1, T2) :-
    time(T1), time(T2),
    T1 = night(N, R1, S1),
    T2 = night(N, R2, S2),
    (R1, S1) < (R2, S2),
    not between_inc(T1, T2).

between_inc(T1, T2) :-
    time(T1), time(T2), time(T3),
    T1 = night(N, R1, S1),
    T2 = night(N, R2, S2),
    T3 = night(N, R3, S3),
    (R1, S1) < (R3, S3),
    (R3, S3) < (R2, S2).

% ===========================================================================
% STATE BRIDGE (maps inc_* inputs to what roles expect)
% ===========================================================================

% Alive status - provided, not derived
alive(P, T) :- inc_alive(P), time(T), player(P).

% Role assignments - CURRENT state used for action validation
% character_assignment_state_at_time is what roles check for their actions
character_assignment_state_at_time(T, P, R) :- inc_role(P, R), time(T).

% Initial assignments - needed by role action rules that check assigned(0, ...)
% We generate time(night(1, 0, 0)) above so that setup rules (like Fortune Teller's
% red herring placement) can fire and pick valid values.
assigned(0, P, R) :- inc_role(P, R).

% Optional override for initial role (e.g., Scarlet Woman became Imp)
assigned(0, P, R) :- inc_initial_role(P, R).

% Received tokens (what players think they are)
% Default: same as role assignment
received(P, R) :- inc_role(P, R), not inc_received(P, _).
received(P, R) :- inc_received(P, R).

% Bag contents - derive from what's assigned
bag(R) :- inc_role(_, R).
bag(R) :- inc_received(_, R).

% Bluffs - if not specified, generate any valid ones
can_be_bluff(R) :- townsfolk(R), not bag(R).
can_be_bluff(R) :- outsider(R), not bag(R).
{ bluff(R) : can_be_bluff(R) } = 3 :- not inc_bluff(_).
bluff(R) :- inc_bluff(R).

% Reminder tokens
reminder_on(Token, P, T) :- inc_reminder(Token, P), time(T).

% Minion alive check (for Imp starpass constraint)
any_minion_alive(T) :- inc_minion_alive, time(T).
% Also derive from state if not explicitly provided
any_minion_alive(T) :-
    not inc_minion_alive_specified,
    inc_role(P, R),
    minion(R),
    inc_alive(P),
    time(T).

% Game is always active in incremental mode (we're validating an action)
game_active(T) :- time(T).

% Functioning (ability works) - default true unless poisoned/drunk
impaired(P, T) :- inc_poisoned(P), time(T).
impaired(P, T) :- reminder_on(Token, P, T), causes_impairment(Token).
functioning(P, T) :- player(P), time(T), not impaired(P, T).

% ===========================================================================
% ROLE CATEGORY HELPERS (needed by some role constraints)
% ===========================================================================

actual_category(P, townsfolk) :- inc_role(P, R), townsfolk(R).
actual_category(P, outsider) :- inc_role(P, R), outsider(R).
actual_category(P, minion) :- inc_role(P, R), minion(R).
actual_category(P, demon) :- inc_role(P, R), demon(R).
actual_category(P, good) :- inc_role(P, R), townsfolk(R).
actual_category(P, good) :- inc_role(P, R), outsider(R).
actual_category(P, evil) :- inc_role(P, R), minion(R).
actual_category(P, evil) :- inc_role(P, R), demon(R).

% Registration (for roles like Recluse)
registers_as(P, C, T) :- actual_category(P, C), time(T), not may_mis_register(P).

% ===========================================================================
% SIGNALS (needed by some roles)
% ===========================================================================

signal(yes).
signal(no).
signal(good).
signal(evil).
signal(count(N)) :- N = 0..player_count.
signal(point(P)) :- player(P).
signal(point(R)) :- role(R).

% ===========================================================================
% SHOW DIRECTIVE
% ===========================================================================

#show player_chooses/4.

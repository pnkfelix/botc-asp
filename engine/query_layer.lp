% =============================================================================
% Query Layer for BotC Engine
% =============================================================================
%
% This file provides generic predicates that the Python engine can query
% to discover abilities, validate transitions, and compute effects.
%
% Role definitions should declare:
%   role_ability(Role, AbilityName, Timing).
%   ability_target_type(AbilityName, TargetType).
%   ability_effect(AbilityName, EffectType).
%
% The Python engine queries:
%   available_ability/3, valid_target/4, transition_valid/0
% =============================================================================

% -----------------------------------------------------------------------------
% Target types (enumeration)
% -----------------------------------------------------------------------------
target_type(alive_player).
target_type(alive_other).
target_type(dead_player).
target_type(any_player).
target_type(neighbor).

% -----------------------------------------------------------------------------
% Timing predicates (when can abilities be used?)
% -----------------------------------------------------------------------------

% An ability timing matches if we're in the right phase
ability_timing_matches(night, T) :- time(T), T = night(N, R, S), night_number(N).
ability_timing_matches(day, T) :- time(T), T = day(N, S).
ability_timing_matches(night(N), T) :- time(T), T = night(N, R, S), night_number(N).
ability_timing_matches(once_per_game, T) :- time(T).  % Further restricted by usage tracking

% First night only
ability_timing_matches(first_night, T) :- time(T), T = night(1, R, S).

% Other nights (not first)
ability_timing_matches(other_nights, T) :- time(T), T = night(N, R, S), N > 1, night_number(N).

% -----------------------------------------------------------------------------
% Target type predicates (who/what can be targeted?)
% -----------------------------------------------------------------------------

% alive_player: any living player
valid_for_target_type(Player, alive_player, T) :-
    player(Player),
    alive(Player, T),
    time(T).

% alive_other: any living player (self-exclusion checked elsewhere)
valid_for_target_type(Player, alive_other, T) :-
    player(Player),
    alive(Player, T),
    time(T).

% dead_player: any dead player
valid_for_target_type(Player, dead_player, T) :-
    player(Player),
    not alive(Player, T),
    time(T).

% any_player: alive or dead
valid_for_target_type(Player, any_player, T) :-
    player(Player),
    time(T).

% neighbor: adjacent in seating (needs seating order facts)
valid_for_target_type(Player, neighbor, T) :-
    player(Player),
    time(T).

% -----------------------------------------------------------------------------
% Ability availability (can a player use an ability right now?)
% -----------------------------------------------------------------------------

% A player can use an ability if:
% 1. They have a role with that ability
% 2. The timing matches the current time
% 3. They're alive (usually)
% 4. They're not blocked (poisoned, drunk, etc. - role-specific)
% 5. They haven't exhausted once-per-game uses

available_ability(Player, Ability, T) :-
    current_time(T),
    assigned(Player, Role, T),
    role_ability(Role, Ability, Timing),
    ability_timing_matches(Timing, T),
    alive(Player, T),
    not ability_blocked(Player, Ability, T),
    not ability_exhausted(Player, Ability, T).

% Ability is blocked if player is poisoned/drunk (for most abilities)
ability_blocked(Player, Ability, T) :-
    poisoned(Player, T),
    role_ability(Role, Ability, _),
    assigned(Player, Role, T),
    not ability_works_while_poisoned(Ability).

ability_blocked(Player, Ability, T) :-
    drunk(Player, T),
    role_ability(Role, Ability, _),
    assigned(Player, Role, T),
    not ability_works_while_drunk(Ability).

% Once-per-game abilities are exhausted after use
ability_exhausted(Player, Ability, T) :-
    role_ability(_, Ability, once_per_game),
    d_ability_used(Player, Ability, _, T2),
    time(T), time(T2),
    T2 < T.

% -----------------------------------------------------------------------------
% Valid targets for an ability
% -----------------------------------------------------------------------------

% For non-"other" target types
valid_target(Player, Ability, Target, T) :-
    available_ability(Player, Ability, T),
    ability_target_type(Ability, TT),
    valid_for_target_type(Target, TT, T),
    TT = alive_player.

valid_target(Player, Ability, Target, T) :-
    available_ability(Player, Ability, T),
    ability_target_type(Ability, TT),
    valid_for_target_type(Target, TT, T),
    TT = dead_player.

valid_target(Player, Ability, Target, T) :-
    available_ability(Player, Ability, T),
    ability_target_type(Ability, TT),
    valid_for_target_type(Target, TT, T),
    TT = any_player.

valid_target(Player, Ability, Target, T) :-
    available_ability(Player, Ability, T),
    ability_target_type(Ability, TT),
    valid_for_target_type(Target, TT, T),
    TT = neighbor.

% For "alive_other" - exclude self
valid_target(Player, Ability, Target, T) :-
    available_ability(Player, Ability, T),
    ability_target_type(Ability, alive_other),
    valid_for_target_type(Target, alive_other, T),
    Player != Target.

% -----------------------------------------------------------------------------
% Transition validation
% -----------------------------------------------------------------------------

% For proposed ability use: check it's valid
proposed_ability_invalid(no_ability) :-
    propose_ability(Player, Ability, Target),
    current_time(T),
    not available_ability(Player, Ability, T).

proposed_ability_invalid(invalid_target) :-
    propose_ability(Player, Ability, Target),
    current_time(T),
    available_ability(Player, Ability, T),
    not valid_target(Player, Ability, Target, T).

% The transition is valid if there are no invalidity reasons
transition_valid :- not proposed_ability_invalid(_).

% Constraint: if checking validity, must be valid
:- check_transition_validity, not transition_valid.

% -----------------------------------------------------------------------------
% Show directives for queries
% -----------------------------------------------------------------------------

#show available_ability/3.
#show valid_target/4.
#show transition_valid/0.
#show proposed_ability_invalid/1.

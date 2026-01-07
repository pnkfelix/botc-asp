% ===========================================================================
% Game End Conditions
% ===========================================================================
% The game ends when either Good or Evil wins.
%
% Good wins when:
%   - All players with demon roles are dead
%     (handles execution, starpass demon dying, multiple demon deaths, etc.)
%
% Evil wins when:
%   - Only 2 players remain alive and the Demon is one of them
%
% Future: Mastermind, Evil Twin, and other special win conditions
% ===========================================================================

% ---------------------------------------------------------------------------
% Fine-Grained Demon Role Tracking
% ---------------------------------------------------------------------------
% Use character_assignment_state_at_time from PR #192 to track who has
% a demon role at each time point.

% has_demon_role(P, T): Player P has a demon role at time T
has_demon_role(P, T) :-
    character_assignment_state_at_time(T, P, R),
    demon(R).

% any_alive_demon(T): There exists an alive player with a demon role at time T
any_alive_demon(T) :-
    has_demon_role(P, T),
    alive(P, T).

% ---------------------------------------------------------------------------
% Good Wins: All demons are dead
% ---------------------------------------------------------------------------

% Good wins at the moment a demon dies AND no alive demons remain.
% This handles all cases: execution, night kills, self-kills, etc.
good_wins(T) :-
    died(P, T),
    has_demon_role(P, T),
    not any_alive_demon(T).

% ---------------------------------------------------------------------------
% Evil Wins: Two or fewer players alive
% ---------------------------------------------------------------------------

% Count of alive players at start of day
alive_at_day_start(N, C) :-
    day_number(N),
    C = #count { P : alive(P, day(N, 0)) }.

% Evil wins at start of day if 2 or fewer alive and a demon is among them
evil_wins(day(N, 0)) :-
    day_number(N),
    alive_at_day_start(N, C), C <= 2,
    any_alive_demon(day(N, 0)).

% ---------------------------------------------------------------------------
% Game Over: Derived from win conditions
% ---------------------------------------------------------------------------

% Game is over at the moment someone wins
game_over(T) :- good_wins(T).
game_over(T) :- evil_wins(T).

% Game over propagates forward through next (for any remaining time points)
game_over(T2) :- game_over(T1), next(T1, T2).

% Game over also propagates based on day number comparison
% This is needed because 'next' is conditional on game_over, which could break propagation
% We use time(T) to ground T, then extract the day number via unification

% Evil wins at day(N, 0): game ends immediately, affects rest of day N and all later times
game_over(day(N, exec)) :- evil_wins(day(N, 0)).
game_over(T) :- evil_wins(day(N, 0)), time(T), T = night(N2, R, S), N2 > N.
game_over(T) :- evil_wins(day(N, 0)), time(T), T = dawn(N2), N2 > N.
game_over(T) :- evil_wins(day(N, 0)), time(T), T = day(N2, P), N2 > N.

% Good wins at day(N, exec): game ends at execution, affects all times after day N
game_over(T) :- good_wins(day(N, exec)), time(T), T = night(N2, R, S), N2 > N.
game_over(T) :- good_wins(day(N, exec)), time(T), T = dawn(N2), N2 > N.
game_over(T) :- good_wins(day(N, exec)), time(T), T = day(N2, P), N2 > N.

% Good wins at night(N, R, S): game ends at that point, affects all times after
% Rest of same night (later substeps)
game_over(T) :- good_wins(T1), T1 = night(N, R1, S1), time(T), T = night(N, R2, S2), (R1, S1) < (R2, S2).
% Dawn and day of same round
game_over(dawn(N)) :- good_wins(T1), T1 = night(N, _, _), day_number(N).
game_over(day(N, P)) :- good_wins(T1), T1 = night(N, _, _), day_time(day(N, P)).
% All later rounds
game_over(T) :- good_wins(T1), T1 = night(N, _, _), time(T), T = night(N2, R, S), N2 > N.
game_over(T) :- good_wins(T1), T1 = night(N, _, _), time(T), T = dawn(N2), N2 > N.
game_over(T) :- good_wins(T1), T1 = night(N, _, _), time(T), T = day(N2, P), N2 > N.

% ---------------------------------------------------------------------------
% Game Over Event (delta predicate for timeline rendering)
% ---------------------------------------------------------------------------

% d_game_over/2 is an explicit event marking when the game ends
% Format: d_game_over(Time, Winner) where Winner is 'good' or 'evil'
d_game_over(T, good) :- good_wins(T).
d_game_over(T, evil) :- evil_wins(T).

% ---------------------------------------------------------------------------
% Convenience predicates for UI/debugging
% ---------------------------------------------------------------------------

% Who won and when
winner(good, T) :- good_wins(T).
winner(evil, T) :- evil_wins(T).

% Game is active (not yet over) at time T
game_active(T) :- time(T), not game_over(T).

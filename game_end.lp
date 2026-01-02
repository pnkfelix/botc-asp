% ===========================================================================
% Game End Conditions
% ===========================================================================
% The game ends when either Good or Evil wins.
%
% Good wins when:
%   - The Demon dies and no one becomes the new Demon
%     (no Scarlet Woman trigger, no starpass)
%
% Evil wins when:
%   - Only 2 players remain alive and the Demon is one of them
%
% Future: Mastermind, Evil Twin, and other special win conditions
% ===========================================================================

% ---------------------------------------------------------------------------
% Good Wins: Demon dies unreplaced
% ---------------------------------------------------------------------------

% The demon dies unreplaced when executed and no one becomes the new demon
% Note: starpass only triggers from self-kill at night, not from execution
% Must check current_demon, not original assignment, to handle SW becoming demon
demon_dies_unreplaced(N) :-
    day_number(N),
    executed(P, N),
    current_demon(P, N),
    not sw_becomes_demon(_, N).

% Good wins at the moment the demon is executed without replacement
good_wins(day(N, exec)) :- demon_dies_unreplaced(N).

% ---------------------------------------------------------------------------
% Evil Wins: Two or fewer players alive
% ---------------------------------------------------------------------------

% Count of alive players at start of day
alive_at_day_start(N, C) :-
    day_number(N),
    C = #count { P : alive(P, day(N, 0)) }.

% Evil wins at start of day if 2 or fewer alive and demon is among them
evil_wins(day(N, 0)) :-
    day_number(N),
    alive_at_day_start(N, C), C <= 2,
    alive(D, day(N, 0)),
    current_demon(D, N).

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
winner(good, T) :- good_wins(T), not evil_wins(T2) : T2 = day(N, 0), good_wins(day(N, exec)).
winner(evil, T) :- evil_wins(T).

% Game is active (not yet over) at time T
game_active(T) :- time(T), not game_over(T).

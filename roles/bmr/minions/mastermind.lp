% Mastermind
% If the Demon dies by execution (ending the game), play for 1 more day. If a player is then executed, their team loses.

% Mastermind triggers when demon is executed and it would normally end the game
mastermind_triggers(N) :-
    day_number(N),
    executed(Demon, N),
    assigned(0, Demon, D), demon(D),
    assigned(0, Mastermind, mastermind),
    alive(Mastermind, day(N, exec)),
    functioning(Mastermind, day(N, exec)),
    not sw_becomes_demon(_, N).  % No Scarlet Woman takeover

% When Mastermind triggers, the game continues for one more day
mastermind_active(N) :- mastermind_triggers(N).

% Override normal good_wins from demon execution when Mastermind is active
:- good_wins(day(N, exec)), mastermind_active(N).

% The game continues to the next day
% (This prevents game_over from triggering at day(N, exec))

% Track the Mastermind day (the extra day after demon death)
mastermind_day(N+1) :- mastermind_triggers(N), day_number(N+1).

% On Mastermind day, if anyone is executed:
% - If good player executed: Evil wins
% - If evil player executed: Good wins
mastermind_execution(P, N) :-
    mastermind_day(N),
    executed(P, N).

evil_wins(day(N, exec)) :-
    mastermind_execution(P, N),
    actual_category(P, good).

good_wins(day(N, exec)) :-
    mastermind_execution(P, N),
    actual_category(P, evil).

% If no execution on Mastermind day, evil wins at end of day
% (This requires tracking whether an execution happened)
no_execution_on_day(N) :-
    day_number(N),
    not executed(_, N).

evil_wins(day(N, exec)) :-
    mastermind_day(N),
    no_execution_on_day(N).

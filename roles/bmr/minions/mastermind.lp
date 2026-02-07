% Mastermind
% If the Demon dies by execution (ending the game), play for 1 more day. If a player is then executed, their team loses.

% Helper: Mastermind fires when a demon is executed, Mastermind is alive + functioning, and no SW takeover
mastermind_triggers(N) :-
    day_number(N),
    executed(Demon, N),
    assigned(0, Demon, D), demon(D),
    assigned(0, Mastermind, mastermind),
    alive(Mastermind, day(N, exec)),
    functioning(Mastermind, day(N, exec)),
    not sw_becomes_demon(_, N).  % No Scarlet Woman takeover

% Helper: marks this day as having the Mastermind trigger active (used to suppress good_wins)
mastermind_active(N) :- mastermind_triggers(N).

% Constraint: good team cannot win from demon execution when Mastermind is active (overrides base rule)
:- good_wins(day(N, exec)), mastermind_active(N).

% Helper: identifies the extra day after demon death as the "Mastermind day"
mastermind_day(N+1) :- mastermind_triggers(N), day_number(N+1).

% Helper: identifies who was executed on the Mastermind day (drives win condition below)
mastermind_execution(P, N) :-
    mastermind_day(N),
    executed(P, N).

% Evil wins if a good player is executed on Mastermind day (separate from good_wins: different alignment)
evil_wins(day(N, exec)) :-
    mastermind_execution(P, N),
    actual_category(P, good).

% Good wins if an evil player is executed on Mastermind day (separate from evil_wins: different alignment)
good_wins(day(N, exec)) :-
    mastermind_execution(P, N),
    actual_category(P, evil).

% Helper: true when no execution occurred on a given day (used for Mastermind no-execution fallback)
no_execution_on_day(N) :-
    day_number(N),
    not executed(_, N).

% Evil wins if no one is executed on the Mastermind day (town failed to act)
evil_wins(day(N, exec)) :-
    mastermind_day(N),
    no_execution_on_day(N).

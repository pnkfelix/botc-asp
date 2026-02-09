% Pacifist
% Executed good players might not die.

% Helper: identifies when a good player was executed (trigger condition for Pacifist save)
good_player_executed(P, N) :-
    executed(P, N),
    actual_category(P, good).

% ST may optionally save the executed good player if Pacifist is alive and functioning
{ pacifist_saves(P, N) } :-
    good_player_executed(P, N),
    assigned(0, Pacifist, pacifist),
    alive(Pacifist, day(N, 0)),
    functioning(Pacifist, day(N, exec)).

% Constraint: saved player cannot die from execution (unless another cause killed them that day)
:- pacifist_saves(P, N), died(P, day(N, exec)), not died_other_cause(P, N).

% Helper: identifies deaths from non-execution causes on the same day (allows constraint exception)
died_other_cause(P, N) :- died(P, day(N, 0)).

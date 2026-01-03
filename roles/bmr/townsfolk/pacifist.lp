% Pacifist
% Executed good players might not die.

% The Pacifist ability is a soft effect - ST may choose to save good players
% This is modeled as a choice for each execution of a good player

% Check if a good player was executed
good_player_executed(P, N) :-
    executed(P, N),
    actual_category(P, good).

% If Pacifist is in play and functioning, good players might not die from execution
{ pacifist_saves(P, N) } :-
    good_player_executed(P, N),
    assigned(0, Pacifist, pacifist),
    alive(Pacifist, day(N, 0)),
    functioning(Pacifist, day(N, exec)).

% If saved, they don't die from execution
% This requires modifying how execution death works
% We need to not derive died(P, day(N, exec)) when pacifist_saves(P, N)

% Override the base execution death rule for saved players
:- pacifist_saves(P, N), died(P, day(N, exec)), not died_other_cause(P, N).

% Marker for deaths from other causes on execution day
died_other_cause(P, N) :- died(P, day(N, 0)).

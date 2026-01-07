% Saint
% "If you are executed, your team loses."
%
% The Saint is an Outsider. If executed while their ability is functioning,
% whatever team the Saint is on loses:
% - Good Saint executed → Evil wins
% - Evil Saint executed → Good wins (e.g., if Mezepheles turned them evil)
%
% Note: If the Saint is drunk or poisoned when executed, their ability doesn't
% trigger and the game continues normally.

% Evil wins if a Good Saint is executed and their ability is functioning
evil_wins(day(N, exec)) :-
    executed(P, N),
    assigned(0, P, saint),
    alignment(P, good, day(N, exec)),
    functioning(P, day(N, exec)).

% Good wins if an Evil Saint is executed and their ability is functioning
% (e.g., Saint turned evil by Mezepheles)
good_wins(day(N, exec)) :-
    executed(P, N),
    assigned(0, P, saint),
    alignment(P, evil, day(N, exec)),
    functioning(P, day(N, exec)).

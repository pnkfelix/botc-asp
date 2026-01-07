% Saint
% "If you are executed, your team loses."
%
% The Saint is an Outsider (good team). If the Saint is executed while their
% ability is functioning, evil wins immediately.
%
% Note: If the Saint is drunk or poisoned when executed, their ability doesn't
% trigger and the game continues normally.
%
% TODO: When alignment-changing roles (Mezepheles, etc.) are implemented,
% this will need to check actual alignment at time of execution, not just
% initial role category.

% Evil wins if a Good Saint is executed and their ability is functioning
evil_wins(day(N, exec)) :-
    executed(P, N),
    assigned(0, P, saint),
    actual_category(P, good),
    functioning(P, day(N, exec)).

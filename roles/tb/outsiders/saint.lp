% Saint
% "If you are executed, your team loses."
%
% The Saint is an Outsider (good team). If the Saint is executed while their
% ability is functioning, evil wins immediately.
%
% Note: If the Saint is drunk or poisoned when executed, their ability doesn't
% trigger and the game continues normally.

% Evil wins at the moment of execution if the Saint is executed and functioning
evil_wins(day(N, exec)) :-
    executed(P, N),
    assigned(0, P, saint),
    functioning(P, day(N, exec)).

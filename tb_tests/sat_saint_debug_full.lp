% DEBUG TEST: Check the full Saint -> evil_wins chain
% Testing each precondition individually

#const player_count = 5.

#include "../botc.lp".
#include "../tb.lp".

needs_night(1).

% Define the 5 players for this test
name(felix; amanda; rob; taylor; sam).
chair(felix, 0).
chair(amanda, 1).
chair(rob, 2).
chair(taylor, 3).
chair(sam, 4).

% Fixed role assignments
assigned(0, felix, imp).
assigned(0, amanda, poisoner).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, sam, saint).

% Sam must not be poisoned
:- reminder_on(poi_poisoned, sam, _).

% Execute sam
executed(sam, 1).

% Verify all preconditions for the Saint rule:
% evil_wins(day(N, exec)) :-
%     executed(P, N),                           -- P=sam, N=1
%     assigned(0, P, saint),                    -- sam is saint
%     alignment(P, good, day(N, exec)),         -- sam is good
%     functioning(P, day(N, exec)).             -- sam is functioning

% Test precondition 1: executed(sam, 1)
:- not executed(sam, 1).

% Test precondition 2: assigned(0, sam, saint)
:- not assigned(0, sam, saint).

% Test precondition 3: alignment(sam, good, day(1, exec))
:- not alignment(sam, good, day(1, exec)).

% Test precondition 4: functioning(sam, day(1, exec))
:- not functioning(sam, day(1, exec)).

% If all preconditions hold, evil_wins should be derived
:- not evil_wins(day(1, exec)).

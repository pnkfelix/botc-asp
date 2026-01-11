% Test: Poison token should be removed when poisoner becomes a different role
% If a poisoner picks a target and then becomes the imp (via starpass) that same night,
% the poison token should be removed from the grimoire.
%
% The poisoner is no longer a poisoner, so their ability effects should be nullified.
%
% Expected: SATISFIABLE
% If this test becomes UNSATISFIABLE, it means the poison token is incorrectly
% persisting after the poisoner has changed roles.

#const player_count = 5.
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

needs_night(3).

% Setup: 5 players
% Distribution for 5 players: 3 townsfolk, 0 outsiders, 1 minion, 1 demon
assigned(0, amanda, soldier).      % Poisoner's target
assigned(0, rob, mayor).
assigned(0, taylor, slayer).
assigned(0, courtney, imp).        % The original Imp
assigned(0, steph, poisoner).      % Minion - will become new Imp via starpass

% Night 2: Poisoner targets Amanda (poisoner acts at order 1)
player_chooses(poisoner, steph, point(amanda), night(2, 1, 2)).

% Night 2: Imp targets itself - starpass (imp acts at order 4)
player_chooses(imp, courtney, point(courtney), night(2, 4, 2)).

% Verify starpass occurs correctly
:- not imp_starpass_triggered(courtney, 2).
:- not starpass_target(steph, 2).
:- not assigned(3, steph, imp).

% CORE ASSERTION: The poison token on Amanda should be removed by night 3
% Since steph is no longer the poisoner (now the imp), their poison effect should be nullified.
% The token should NOT be on Amanda at the start of night 3.
:- reminder_on(poi_poisoned, amanda, night(3, 0, 0)).

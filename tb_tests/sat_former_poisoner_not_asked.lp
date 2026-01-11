% Test: Former-poisoner should NOT be asked to pick a poison target
% When a poisoner becomes a different role (e.g., imp via starpass),
% they should not be prompted to choose a poison target on subsequent nights.
%
% This test verifies that if a poisoner becomes the imp on night 2,
% they are NOT asked to pick a poison target on night 3.
%
% Expected: SATISFIABLE
% If this test becomes UNSATISFIABLE, it means the former-poisoner is being
% incorrectly asked to pick a poison target.

#const player_count = 5.
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

needs_night(3).

% Setup: 5 players
% Distribution for 5 players: 3 townsfolk, 0 outsiders, 1 minion, 1 demon
assigned(0, amanda, soldier).
assigned(0, rob, mayor).
assigned(0, taylor, slayer).
assigned(0, courtney, imp).       % The original Imp
assigned(0, steph, poisoner).     % Minion - will become new Imp via starpass

% Poisoner won't target their own demon
:- player_chooses(poisoner, _, point(Demon), _), assigned(0, Demon, imp).

% Night 2: Imp targets itself (starpass)
player_chooses(imp, courtney, point(courtney), night(2, 4, 2)).

% Verify starpass occurs correctly
:- not imp_starpass_triggered(courtney, 2).
:- not starpass_target(steph, 2).
:- not assigned(3, steph, imp).

% CORE ASSERTION: Former-poisoner (steph) should NOT be asked to pick a target on night 3
% The former poisoner is now the Imp, so they should not act as poisoner.
% If player_chooses(poisoner, steph, ..., night(3, ...)) exists, this constraint fails.
:- player_chooses(poisoner, steph, _, night(3, _, 2)).

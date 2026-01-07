% Test: Good wins when demon kills self with no minion for starpass
%
% Scenario:
% - All minions are dead
% - Night 2: Imp kills itself (thinking it has starpass, but no minions)
% - Result: Good wins because the demon died with no replacement
%
% This tests that good_wins triggers on night deaths, not just executions.

#include "base.lp".

needs_night(3).

% 8 players - use the default player_count from base.lp
% TB distribution for 8: 5 townsfolk, 1 outsider, 1 minion, 1 demon
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, chef).
assigned(0, courtney, investigator).
assigned(0, neha, slayer).
assigned(0, pratik, butler).       % Outsider
assigned(0, felix, poisoner).      % Minion - will be dead
assigned(0, steph, imp).           % Demon

% Day 1: Execute the minion (felix dies)
executed(felix, 1).

% Verify felix is dead before night 2
:- not dead(felix, night(2, 0, 0)).

% Night 2: Imp kills itself (no minions alive for starpass)
player_chooses(imp, steph, point(steph), night(2, 4, 2)).

% Verify the imp successfully kills itself
:- not imp_kill_succeeds(steph, night(2, 4, 3)).

% Starpass should NOT trigger (no eligible minions)
:- starpass_target(_, 2).

% Assert: Good wins at the moment the imp dies (night 2)
:- not good_wins(night(2, 4, 3)).

% Test: Good does NOT win when SW takes over after demon execution
%
% Scenario:
% - 6 players (SW trigger threshold is 5)
% - Day 1: The demon is executed
% - Scarlet Woman takes over
% - Result: Good should NOT win because SW is alive with demon role
%
% This is UNSAT because we assert good_wins but SW takes over.

#include "base.lp".

needs_night(2).

% 6 players - above SW threshold
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, sarah, chef).
assigned(0, felix, baron).
assigned(0, steph, scarlet_woman).  % Will become demon
assigned(0, courtney, imp).         % Original demon

% Day 1: Execute the demon
executed(courtney, 1).

% Assert: SW takes over
:- not sw_becomes_demon(steph, 1).

% Assert: steph should have demon role after takeover
:- not has_demon_role(steph, day(1, exec)).

% Assert: There IS an alive demon (steph)
:- not any_alive_demon(day(1, exec)).

% This makes it UNSAT: assert good wins, but they shouldn't
:- not good_wins(day(1, exec)).

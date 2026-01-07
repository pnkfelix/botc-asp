% Test: Good wins when an Evil Saint is executed
% The Saint's ability: "If you are executed, your team loses."
% If the Saint has been turned evil (e.g., by Mezepheles), executing them
% causes the evil team to lose, meaning good wins.
%
% Scenario: 5 players
% - felix (imp), amanda (poisoner), rob (washerwoman), taylor (chef), sam (saint)
% - Sam (Saint) has been turned evil by some effect (simulated via alignment_changed)
% - Day 1: Town executes sam (the now-Evil Saint)
% - Good wins at day(1, exec) because Saint's team (evil) loses

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

% Simulate: Saint was turned evil (e.g., by Mezepheles on night 1)
% This overrides the default good alignment from being an Outsider
alignment_changed(sam, evil, T) :- time(T), T = day(_, _).
alignment_changed(sam, evil, T) :- time(T), T = night(N, _, _), N > 1.

% Saint must not be poisoned (so their ability is functioning)
:- reminder_on(poi_poisoned, sam, _).

% Day 1: Execute the Saint (who is now evil)
executed(sam, 1).

% Good should win at the execution (Saint's team = evil loses)
:- not good_wins(day(1, exec)).

% Verify d_game_over event fires for good
:- not d_game_over(day(1, exec), good).

% Verify game is over after the execution
:- not game_over(day(1, exec)).

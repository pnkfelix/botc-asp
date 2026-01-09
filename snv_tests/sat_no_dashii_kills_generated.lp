% Test: No Dashii kills on night 2 (solver generates the choice)
% This test verifies the demon kill generation rule fires correctly

#include "base.lp".

needs_night(2).

% Force specific role assignments
assert_received(amanda, clockmaker).
assert_received(rob, dreamer).
assert_received(taylor, oracle).
assert_received(steph, no_dashii).
assert_received(felix, cerenovus).

% Night 1: No one dies (first night has no demon kill)
% Day 1: No execution
% Night 2: No Dashii should choose and kill someone

% Assert: at least one player must die from the demon kill
:- not died(_, _).

% Assert: No Dashii (steph) should survive (didn't self-target)
:- died(steph, _).

% Assert: The death should be from No Dashii's attack
:- died(P, _), not reminder_on(no_dashii_dead, P, _).

% Test: 15 players should have 2 base outsiders
#include "../botc.lp".
#include "../tb.lp".
#include "../players.lp".

#const player_count = 15.

% Force 2 outsiders
assigned(0, felix, drunk).
assigned(0, kunjal, butler).

% No baron
:- assigned(0, _, baron).

% ====================================================================
% Additional constraints to speed up solving:
% Pin the demon and minion assignments to reduce search space.
% The core test (15 players = 2 base outsiders) is unaffected by which
% specific players hold these roles.
% ====================================================================

% Fix demon assignment
assigned(0, amanda, imp).

% Fix minion assignments (all 3 non-baron minions required for 15 players)
assigned(0, rob, poisoner).
assigned(0, taylor, spy).
assigned(0, courtney, scarlet_woman).

% Fix which 9 townsfolk are in play (eliminates C(13,9)=715 combinations)
% This doesn't affect the outsider count test.
assert_distrib(washerwoman).
assert_distrib(librarian).
assert_distrib(investigator).
assert_distrib(chef).
assert_distrib(empath).
assert_distrib(fortune_teller).
assert_distrib(undertaker).
assert_distrib(monk).
assert_distrib(slayer).

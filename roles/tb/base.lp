% Trouble Brewing - Base Definitions

% Promote TB-specific role atoms into the generic category predicates used by botc.lp
townsfolk(X) :- tb_townsfolk(X).
outsider(X) :- tb_outsider(X).
minion(X) :- tb_minion(X).
demon(X) :- tb_demon(X).

% Role roster: each atom is defined here so #include "tb.lp" brings the full script
tb_townsfolk(washerwoman; librarian; investigator; chef; empath; fortune_teller; undertaker; monk; ravenkeeper; virgin; slayer; soldier; mayor).
tb_outsider(butler; drunk; recluse; saint).
tb_minion(poisoner; spy; scarlet_woman; baron).
tb_demon(imp).

% First Night Order - determines time-point generation and substep sequencing for night 1
first_night_role_order(poisoner, 1).
first_night_role_order(washerwoman, 2).
first_night_role_order(librarian, 3).
first_night_role_order(investigator, 4).
first_night_role_order(chef, 5).
first_night_role_order(empath, 6).
first_night_role_order(fortune_teller, 7).
first_night_role_order(butler, 8).
first_night_role_order(spy, 9).

% Other Night Order - separate from first-night because some roles only act on subsequent nights
other_night_role_order(poisoner, 1).
other_night_role_order(monk, 2).
other_night_role_order(scarlet_woman, 3).
other_night_role_order(imp, 4).
other_night_role_order(ravenkeeper, 5).
other_night_role_order(empath, 6).
other_night_role_order(fortune_teller, 7).
other_night_role_order(undertaker, 8).
other_night_role_order(butler, 9).
other_night_role_order(spy, 10).

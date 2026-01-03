% Sects & Violets - Base Definitions

townsfolk(X) :- snv_townsfolk(X).
outsider(X) :- snv_outsider(X).
minion(X) :- snv_minion(X).
demon(X) :- snv_demon(X).

snv_townsfolk(clockmaker; dreamer; snake_charmer; mathematician; flowergirl; town_crier; oracle; savant; seamstress; philosopher; artist; juggler; sage).
snv_outsider(mutant; sweetheart; barber; klutz).
snv_minion(evil_twin; witch; cerenovus; pit_hag).
snv_demon(fang_gu; vigormortis; vortox; no_dashii).

% First Night Order
first_night_role_order(philosopher, 1).
first_night_role_order(snake_charmer, 2).
first_night_role_order(evil_twin, 3).
first_night_role_order(witch, 4).
first_night_role_order(cerenovus, 5).
first_night_role_order(clockmaker, 6).
first_night_role_order(dreamer, 7).
first_night_role_order(mathematician, 8).
first_night_role_order(flowergirl, 9).
first_night_role_order(town_crier, 10).
first_night_role_order(oracle, 11).
first_night_role_order(seamstress, 12).

% Other Night Order
other_night_role_order(philosopher, 1).
other_night_role_order(snake_charmer, 2).
other_night_role_order(witch, 3).
other_night_role_order(cerenovus, 4).
other_night_role_order(pit_hag, 5).
other_night_role_order(fang_gu, 6).
other_night_role_order(vigormortis, 7).
other_night_role_order(vortox, 8).
other_night_role_order(no_dashii, 9).
other_night_role_order(dreamer, 10).
other_night_role_order(mathematician, 11).
other_night_role_order(flowergirl, 12).
other_night_role_order(town_crier, 13).
other_night_role_order(oracle, 14).
other_night_role_order(seamstress, 15).
other_night_role_order(sage, 16).
other_night_role_order(barber, 17).
other_night_role_order(sweetheart, 18).

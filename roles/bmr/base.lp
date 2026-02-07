% Bad Moon Rising - Base Definitions

% Promote BMR-specific role atoms into the generic category predicates used by botc.lp
townsfolk(X) :- bmr_townsfolk(X).
outsider(X) :- bmr_outsider(X).
minion(X) :- bmr_minion(X).
demon(X) :- bmr_demon(X).

bmr_townsfolk(grandmother; sailor; chambermaid; exorcist; innkeeper; gambler; gossip; courtier; professor; minstrel; tea_lady; pacifist; fool).
bmr_outsider(tinker; moonchild; goon; lunatic).
bmr_minion(godfather; devils_advocate; assassin; mastermind).
bmr_demon(zombuul; pukka; shabaloth; po).

% First Night Order (based on official night order)
first_night_role_order(lunatic, 1).
first_night_role_order(sailor, 2).
first_night_role_order(courtier, 3).
first_night_role_order(godfather, 4).
first_night_role_order(devils_advocate, 5).
first_night_role_order(pukka, 6).
first_night_role_order(grandmother, 7).
first_night_role_order(chambermaid, 8).
first_night_role_order(goon, 9).

% Other Night Order
other_night_role_order(sailor, 1).
other_night_role_order(innkeeper, 2).
other_night_role_order(courtier, 3).
other_night_role_order(devils_advocate, 4).
other_night_role_order(gambler, 5).
other_night_role_order(exorcist, 6).
other_night_role_order(zombuul, 7).
other_night_role_order(pukka, 8).
other_night_role_order(shabaloth, 9).
other_night_role_order(po, 10).
other_night_role_order(assassin, 11).
other_night_role_order(godfather, 12).
other_night_role_order(gossip, 13).
other_night_role_order(tinker, 14).
other_night_role_order(moonchild, 15).
other_night_role_order(grandmother, 16).
other_night_role_order(professor, 17).
other_night_role_order(chambermaid, 18).
other_night_role_order(minstrel, 19).
other_night_role_order(tea_lady, 20).

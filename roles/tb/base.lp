% Trouble Brewing - Base Definitions

townsfolk(X) :- tb_townsfolk(X).
outsider(X) :- tb_outsider(X).
minion(X) :- tb_minion(X).
demon(X) :- tb_demon(X).

tb_townsfolk(washerwoman; librarian; investigator; chef; empath; fortune_teller; undertaker; monk; ravenkeeper; virgin; slayer; soldier; mayor).
tb_outsider(butler; drunk; recluse; saint).
tb_minion(poisoner; spy; scarlet_woman; baron).
tb_demon(imp).

% Night order is derived from the global night_order.lp file.
% See botc.lp for the derivation rules that create first_night_role_order/2
% and other_night_role_order/2 from night_order/3.

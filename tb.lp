% drunk token is not allowed in the bag.
never_in_bag(drunk).
mistaken_identity(drunk, townsfolk).

townsfolk(X) :- tb_townsfolk(X).
outsider(X) :- tb_outsider(X).
minion(X) :- tb_minion(X).
demon(X) :- tb_demon(X).

tb_townsfolk(washerwoman; librarian; investigator; chef; empath; fortune_teller; undertaker; monk; ravenkeeper; virgin; slayer; soldier; mayor).
tb_outsider(butler; drunk; recluse; saint).

tb_minion(poisoner; spy; scarlet_woman; baron).
tb_demon(imp).

causes_outsider_mod(baron, 2).

% First Night Order for Trouble Brewing
% Role order determines which role acts when
first_night_role_order(poisoner, 1).
first_night_role_order(washerwoman, 2).
first_night_role_order(librarian, 3).
first_night_role_order(investigator, 4).
first_night_role_order(chef, 5).
first_night_role_order(empath, 6).
first_night_role_order(fortune_teller, 7).
first_night_role_order(butler, 8).
first_night_role_order(spy, 9).

% Substeps for each role's first night actions
% first_night_substep(Role, Substep, Action)

% Poisoner: asks who to poison, player chooses, ST places token
first_night_substep(poisoner, 1, st_asks(choose_target)).
first_night_substep(poisoner, 2, player_chooses(target)).
first_night_substep(poisoner, 3, st_places(poisoned)).

% Poisoner reminder token placement
% At substep 3, ST places poisoned token on whoever the poisoner chose
{ reminder_on(poisoner_poisoned, P, night(1, RoleOrd, 3)) : player(P) } = 1 :-
    first_night_role_order(poisoner, RoleOrd),
    assigned(0, _, poisoner).

% The poisoner's token causes impairment
causes_impairment(poisoner_poisoned).

% Washerwoman: ST places tokens then tells info
first_night_substep(washerwoman, 1, st_places(washerwoman_townsfolk)).
first_night_substep(washerwoman, 2, st_places(washerwoman_wrong)).
first_night_substep(washerwoman, 3, st_tells(washerwoman_info)).

% Washerwoman reminder token placement
% At substep 1, ST chooses a player to put the "townsfolk" token on
% At substep 2, ST chooses a different player to put the "wrong" token on

% The storyteller chooses where to place these tokens
{ reminder_on(washerwoman_townsfolk, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, _, washerwoman).

{ reminder_on(washerwoman_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, _, washerwoman).

% The two tokens must be on different players
:- reminder_on(washerwoman_townsfolk, P, T1),
   reminder_on(washerwoman_wrong, P, T2).

% Washerwoman validity constraint:
% If washerwoman is functioning, the townsfolk token must be on someone
% who registers as townsfolk at that time
:- reminder_on(washerwoman_townsfolk, P, T),
   assigned(0, WasherwomanPlayer, washerwoman),
   functioning(WasherwomanPlayer, T),
   not registers_as(P, townsfolk, T).

% ===========================================================================
% Spy and Recluse mis-registration
% ===========================================================================

% Spy can register as good and/or as townsfolk/outsider
may_mis_register(P) :- assigned(0, P, spy).
may_register_as(P, good) :- assigned(0, P, spy).
may_register_as(P, evil) :- assigned(0, P, spy).
may_register_as(P, townsfolk) :- assigned(0, P, spy).
may_register_as(P, outsider) :- assigned(0, P, spy).
may_register_as(P, minion) :- assigned(0, P, spy).

% Recluse can register as evil and/or as minion/demon
may_mis_register(P) :- assigned(0, P, recluse).
may_register_as(P, good) :- assigned(0, P, recluse).
may_register_as(P, evil) :- assigned(0, P, recluse).
may_register_as(P, outsider) :- assigned(0, P, recluse).
may_register_as(P, minion) :- assigned(0, P, recluse).
may_register_as(P, demon) :- assigned(0, P, recluse).


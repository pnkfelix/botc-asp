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
{ reminder_on(poi_poisoned, P, night(1, RoleOrd, 3)) : player(P) } = 1 :-
    first_night_role_order(poisoner, RoleOrd),
    assigned(0, _, poisoner).

% The poisoner's token causes impairment
causes_impairment(poi_poisoned).

% Washerwoman: ST places tokens then tells info
first_night_substep(washerwoman, 1, st_places(ww_townsfolk)).
first_night_substep(washerwoman, 2, st_places(ww_wrong)).
first_night_substep(washerwoman, 3, st_tells(ww_info)).

% Washerwoman reminder token placement
% At substep 1, ST chooses a player to put the "townsfolk" token on
% At substep 2, ST chooses a different player to put the "wrong" token on

% The storyteller chooses where to place these tokens
{ reminder_on(ww_townsfolk, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, _, washerwoman).

{ reminder_on(ww_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, _, washerwoman).

% The two tokens must be on different players
:- reminder_on(ww_townsfolk, P, T1),
   reminder_on(ww_wrong, P, T2).

% Washerwoman validity constraint:
% If washerwoman is functioning, the townsfolk token must be on someone
% who registers as townsfolk at that time
% FIXME: Due to reminder token inertia, this constraint applies at ALL times
% the token exists, not just when the washerwoman receives info. We probably
% only care about the time when st_tells(ww_info) occurs.
:- reminder_on(ww_townsfolk, P, T),
   assigned(0, WasherwomanPlayer, washerwoman),
   functioning(WasherwomanPlayer, T),
   not registers_as(P, townsfolk, T).

% ===========================================================================
% Librarian: like Washerwoman but for outsiders
% ===========================================================================
first_night_substep(librarian, 1, st_places(lib_outsider)).
first_night_substep(librarian, 2, st_places(lib_wrong)).
first_night_substep(librarian, 3, st_tells(lib_info)).

{ reminder_on(lib_outsider, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(librarian, RoleOrd),
    assigned(0, _, librarian).

{ reminder_on(lib_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(librarian, RoleOrd),
    assigned(0, _, librarian).

% The two tokens must be on different players
:- reminder_on(lib_outsider, P, _),
   reminder_on(lib_wrong, P, _).

% If librarian is functioning, the outsider token must be on someone
% who registers as outsider at that time
% FIXME: Same issue as washerwoman - applies at all times, not just info time
:- reminder_on(lib_outsider, P, T),
   assigned(0, LibrarianPlayer, librarian),
   functioning(LibrarianPlayer, T),
   not registers_as(P, outsider, T).

% ===========================================================================
% Investigator: like Washerwoman but for minions
% ===========================================================================
first_night_substep(investigator, 1, st_places(inv_minion)).
first_night_substep(investigator, 2, st_places(inv_wrong)).
first_night_substep(investigator, 3, st_tells(inv_info)).

{ reminder_on(inv_minion, P, night(1, RoleOrd, 1)) : player(P) } = 1 :-
    first_night_role_order(investigator, RoleOrd),
    assigned(0, _, investigator).

{ reminder_on(inv_wrong, P, night(1, RoleOrd, 2)) : player(P) } = 1 :-
    first_night_role_order(investigator, RoleOrd),
    assigned(0, _, investigator).

% The two tokens must be on different players
:- reminder_on(inv_minion, P, _),
   reminder_on(inv_wrong, P, _).

% If investigator is functioning, the minion token must be on someone
% who registers as minion at that time
% FIXME: Same issue as washerwoman - applies at all times, not just info time
:- reminder_on(inv_minion, P, T),
   assigned(0, InvestigatorPlayer, investigator),
   functioning(InvestigatorPlayer, T),
   not registers_as(P, minion, T).

% ===========================================================================
% Chef: learns how many pairs of evil players are neighbors
% ===========================================================================
first_night_substep(chef, 1, st_tells(chef_info)).

% Chef info is derived from grimoire state, no reminder tokens needed
% The ST tells the chef a number; if functioning, it must be accurate

% ===========================================================================
% Empath: learns how many of their alive neighbors are evil
% ===========================================================================
first_night_substep(empath, 1, st_tells(empath_info)).

% Empath info is derived from grimoire state, no reminder tokens needed
% The ST tells the empath a number; if functioning, it must be accurate

% ===========================================================================
% Fortune Teller: chooses 2 players, learns if either is the demon
% ===========================================================================
first_night_substep(fortune_teller, 1, st_asks(choose_two)).
first_night_substep(fortune_teller, 2, player_chooses(two_players)).
first_night_substep(fortune_teller, 3, st_tells(fortune_teller_info)).

% Fortune teller has a "red herring" - one player who always registers as demon to them
% This is placed during setup, not during the fortune teller's turn
% For now, we'll model this as a reminder token placed at time 0

{ reminder_on(ft_red_herring, P, night(1, 0, 0)) : player(P) } = 1 :-
    assigned(0, _, fortune_teller).

% Red herring cannot be on the actual demon
:- reminder_on(ft_red_herring, P, _),
   assigned(0, P, R),
   demon(R).

% Red herring must be on a good player (townsfolk or outsider)
:- reminder_on(ft_red_herring, P, T),
   not registers_as(P, good, T).

% ===========================================================================
% Butler: chooses a master, can only vote when master votes
% ===========================================================================
first_night_substep(butler, 1, st_asks(choose_master)).
first_night_substep(butler, 2, player_chooses(master)).
first_night_substep(butler, 3, st_places(but_master)).

{ reminder_on(but_master, P, night(1, RoleOrd, 3)) : player(P) } = 1 :-
    first_night_role_order(butler, RoleOrd),
    assigned(0, _, butler).

% Butler cannot choose themselves as master
:- reminder_on(but_master, P, _),
   assigned(0, P, butler).

% ===========================================================================
% Spy: sees the grimoire (no mechanical effect to model here)
% ===========================================================================
first_night_substep(spy, 1, st_shows(grimoire)).

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


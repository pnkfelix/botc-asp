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

% Poisoner player choice: choose any other player to poison
{ player_chooses(poisoner, Poisoner, point(P), T) : player(P), P != Poisoner } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(poisoner, RoleOrd),
    assigned(0, Poisoner, poisoner).

% Poisoner reminder token placement: ST places token on the chosen player
reminder_on(poi_poisoned, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(poisoner, RoleOrd),
    player_chooses(poisoner, _, point(P), night(1, RoleOrd, 2)).

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

% Washerwoman st_tells: ST points to two players and a role
% The two players are derived from the reminder tokens
st_tells(washerwoman, WW, point(P1), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, WW, washerwoman),
    reminder_on(ww_townsfolk, P1, T).

st_tells(washerwoman, WW, point(P2), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, WW, washerwoman),
    reminder_on(ww_wrong, P2, T).

% The role shown depends on whether washerwoman is functioning
% If functioning: must be a townsfolk role the ww_townsfolk player can register as
% If malfunctioning: can be any townsfolk role

% A player can register as role R if:
% - R is their actual assigned role, OR
% - They can misregister as R's category (spy/recluse only)

% Everyone can register as their actual role
may_register_as_role(P, R, T) :-
    assigned(0, P, R),
    time(T).

% Spy/Recluse can register as any role in a category they may misregister as
may_register_as_role(P, R, T) :-
    townsfolk(R),
    may_misregister_as(P, townsfolk),
    time(T).

% When functioning, role must match what ww_townsfolk player can register as
{ st_tells(washerwoman, WW, point(R), T) : may_register_as_role(P, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, WW, washerwoman),
    reminder_on(ww_townsfolk, P, T),
    functioning(WW, T).

% When malfunctioning, ST can say any townsfolk role
{ st_tells(washerwoman, WW, point(R), T) : townsfolk(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(washerwoman, RoleOrd),
    assigned(0, WW, washerwoman),
    not functioning(WW, T).

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

% Librarian st_tells: ST points to two players and a role
st_tells(librarian, LIB, point(P1), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    reminder_on(lib_outsider, P1, T).

st_tells(librarian, LIB, point(P2), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    reminder_on(lib_wrong, P2, T).

% Spy/Recluse can register as any outsider role
may_register_as_role(P, R, T) :-
    outsider(R),
    may_misregister_as(P, outsider),
    time(T).

% When functioning, role must match what lib_outsider player can register as
{ st_tells(librarian, LIB, point(R), T) : may_register_as_role(P, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    reminder_on(lib_outsider, P, T),
    functioning(LIB, T).

% When malfunctioning, ST can say any outsider role
{ st_tells(librarian, LIB, point(R), T) : outsider(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(librarian, RoleOrd),
    assigned(0, LIB, librarian),
    not functioning(LIB, T).

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

% Investigator st_tells: ST points to two players and a role
st_tells(investigator, INV, point(P1), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    reminder_on(inv_minion, P1, T).

st_tells(investigator, INV, point(P2), T) :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    reminder_on(inv_wrong, P2, T).

% Recluse can register as any minion role
may_register_as_role(P, R, T) :-
    minion(R),
    may_misregister_as(P, minion),
    time(T).

% When functioning, role must match what inv_minion player can register as
{ st_tells(investigator, INV, point(R), T) : may_register_as_role(P, R, T) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    reminder_on(inv_minion, P, T),
    functioning(INV, T).

% When malfunctioning, ST can say any minion role
{ st_tells(investigator, INV, point(R), T) : minion(R) } = 1 :-
    T = night(1, RoleOrd, 3),
    first_night_role_order(investigator, RoleOrd),
    assigned(0, INV, investigator),
    not functioning(INV, T).

% ===========================================================================
% Chef: learns how many pairs of evil players are neighbors
% ===========================================================================
first_night_substep(chef, 1, st_tells(chef_info)).

% Chef info is derived from grimoire state, no reminder tokens needed
% The ST tells the chef a number; if functioning, it must be accurate

% An "evil pair" is two neighboring players who both register as evil
% We count ordered pairs where P1 < P2 (by chair) to avoid double-counting
evil_pair(P1, P2, T) :-
    neighbor(P1, P2),
    chair(P1, C1), chair(P2, C2), C1 < C2,
    registers_as(P1, evil, T),
    registers_as(P2, evil, T).

% Count the number of evil pairs at time T
evil_pair_count(N, T) :-
    time(T),
    N = #count { P1, P2 : evil_pair(P1, P2, T) }.

% Chef st_tells: ST tells the chef a count
% When functioning, the count must match the actual evil_pair_count
st_tells(chef, Chef, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(chef, RoleOrd),
    assigned(0, Chef, chef),
    functioning(Chef, T),
    evil_pair_count(N, T).

% When malfunctioning, ST can tell any count from 0 to player_count/2
{ st_tells(chef, Chef, count(N), T) : N = 0..player_count/2 } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(chef, RoleOrd),
    assigned(0, Chef, chef),
    not functioning(Chef, T).

% ===========================================================================
% Empath: learns how many of their alive neighbors are evil
% ===========================================================================
first_night_substep(empath, 1, st_tells(empath_info)).

% Empath info is derived from grimoire state, no reminder tokens needed
% The ST tells the empath a number; if functioning, it must be accurate

% Count how many of the empath's neighbors register as evil
% Note: On night 1, everyone is alive, so we don't need to check alive status yet
empath_evil_neighbor_count(Empath, N, T) :-
    assigned(0, Empath, empath),
    time(T),
    N = #count { P : neighbor(Empath, P), registers_as(P, evil, T) }.

% Empath st_tells: ST tells the empath a count (0, 1, or 2)
% When functioning, the count must match the actual neighbor evil count
st_tells(empath, Empath, count(N), T) :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(empath, RoleOrd),
    assigned(0, Empath, empath),
    functioning(Empath, T),
    empath_evil_neighbor_count(Empath, N, T).

% When malfunctioning, ST can tell any count from 0 to 2
{ st_tells(empath, Empath, count(N), T) : N = 0..2 } = 1 :-
    T = night(1, RoleOrd, 1),
    first_night_role_order(empath, RoleOrd),
    assigned(0, Empath, empath),
    not functioning(Empath, T).

% ===========================================================================
% Fortune Teller: chooses 2 players, learns if either is the demon
% Acts on first night and every other night
% ===========================================================================
first_night_substep(fortune_teller, 1, st_asks(choose_two)).
first_night_substep(fortune_teller, 2, player_chooses(two_players)).
first_night_substep(fortune_teller, 3, st_tells(ft_info)).

other_night_substep(fortune_teller, 1, st_asks(choose_two)).
other_night_substep(fortune_teller, 2, player_chooses(two_players)).
other_night_substep(fortune_teller, 3, st_tells(ft_info)).

% Other Night Order for Trouble Brewing
% Role order determines which role acts when (on nights 2+)
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

% Fortune teller has a "red herring" - one player who always registers as demon to them
% This is placed during setup, not during the fortune teller's turn

{ reminder_on(ft_red_herring, P, night(1, 0, 0)) : player(P) } = 1 :-
    assigned(0, _, fortune_teller).

% Red herring cannot be on the actual demon
:- reminder_on(ft_red_herring, P, _),
   assigned(0, P, R),
   demon(R).

% Red herring must be on a good player (townsfolk or outsider)
:- reminder_on(ft_red_herring, P, T),
   not registers_as(P, good, T).

% Fortune teller chooses 2 different players (unordered set)
% ft_choice(FT, P, T) means FT chose player P at time T
{ ft_choice(FT, P, T) : player(P) } = 2 :-
    assigned(0, FT, fortune_teller),
    acting_role(T, fortune_teller).

% Convert to player_chooses signals (one for each chosen player)
player_chooses(fortune_teller, FT, point(P), T) :-
    ft_choice(FT, P, T).

% A player "pings as demon" to the fortune teller if:
% - They are the actual demon, OR
% - They have the red herring token
ft_pings_as_demon(P, T) :-
    assigned(0, P, R),
    demon(R),
    time(T).

ft_pings_as_demon(P, T) :-
    reminder_on(ft_red_herring, P, T).

% Helper: FT got a ping if any chosen player pings
ft_got_ping(FT, T) :-
    ft_choice(FT, P, T),
    ft_pings_as_demon(P, T).

% Fortune teller st_tells: yes if either chosen player pings as demon
% When functioning: answer must be accurate
st_tells(fortune_teller, FT, yes, T) :-
    acting_role(T, fortune_teller),
    assigned(0, FT, fortune_teller),
    functioning(FT, T),
    ft_got_ping(FT, T).

st_tells(fortune_teller, FT, no, T) :-
    acting_role(T, fortune_teller),
    assigned(0, FT, fortune_teller),
    functioning(FT, T),
    not ft_got_ping(FT, T).

% When malfunctioning: ST can say yes or no
{ st_tells(fortune_teller, FT, yes, T) ; st_tells(fortune_teller, FT, no, T) } = 1 :-
    acting_role(T, fortune_teller),
    assigned(0, FT, fortune_teller),
    not functioning(FT, T).

% ===========================================================================
% Butler: chooses a master, can only vote when master votes
% ===========================================================================
first_night_substep(butler, 1, st_asks(choose_master)).
first_night_substep(butler, 2, player_chooses(master)).
first_night_substep(butler, 3, st_places(but_master)).

% Butler player choice: choose any other player as master
{ player_chooses(butler, Butler, point(P), T) : player(P), P != Butler } = 1 :-
    T = night(1, RoleOrd, 2),
    first_night_role_order(butler, RoleOrd),
    assigned(0, Butler, butler).

% Butler reminder token placement: ST places token on the chosen master
reminder_on(but_master, P, night(1, RoleOrd, 3)) :-
    first_night_role_order(butler, RoleOrd),
    player_chooses(butler, _, point(P), night(1, RoleOrd, 2)).

% Butler voting restriction: Butler can only vote when master votes
% This is a constraint that makes scenarios UNSAT if butler votes without master
:- assigned(0, Butler, butler),
   vote(Butler, N, K),
   reminder_on(but_master, Master, _),
   not vote(Master, N, K).

% ===========================================================================
% Virgin: if a Townsfolk nominates the Virgin, that Townsfolk is executed
% ===========================================================================

% Virgin ability triggers when:
% 1. Virgin is functioning
% 2. Nominator registers as Townsfolk
% The nominator dies immediately and the day ends
virgin_triggered(Nominator, N, K) :-
    nomination(Nominator, Virgin, N, K),
    assigned(0, Virgin, virgin),
    functioning(Virgin, day(N, nom(K))),
    registers_as(Nominator, townsfolk, day(N, nom(K))).

% The nominator is executed when virgin is triggered
executed(Nominator, N) :- virgin_triggered(Nominator, N, K).

% Virgin triggering ends the day (counts as execution for day ending purposes)
day_ends(N) :- virgin_triggered(_, N, _).

% Virgin ability is used up after first trigger (one-shot ability)
% Mark with reminder token so it can't trigger again
reminder_on(vir_no_ability, Virgin, day(N, nom(K))) :-
    virgin_triggered(_, N, K),
    assigned(0, Virgin, virgin).

% Virgin doesn't trigger if ability already used
:- virgin_triggered(_, N1, K1),
   virgin_triggered(_, N2, K2),
   (N1, K1) != (N2, K2).

% ===========================================================================
% Slayer: once per game, publicly chooses a player; if demon, they die
% ===========================================================================

% slayer_shot(Slayer, Target, Day) - Slayer uses ability on Target during Day
% This is input from the scenario

% Validate: Slayer must be assigned the slayer role
:- slayer_shot(Slayer, _, _), not assigned(0, Slayer, slayer).

% Validate: Both must be alive
:- slayer_shot(Slayer, _, N), not alive(Slayer, day(N, 0)).
:- slayer_shot(_, Target, N), not alive(Target, day(N, 0)).

% Slayer ability works if Slayer is functioning and target is the demon
slayer_kills(Slayer, Target, N) :-
    slayer_shot(Slayer, Target, N),
    functioning(Slayer, day(N, 0)),
    assigned(0, Target, R),
    demon(R).

% Target dies if slayer ability works
died(Target, day(N, 0)) :- slayer_kills(_, Target, N).

% Slayer can only use ability once per game
:- slayer_shot(S, _, N1), slayer_shot(S, _, N2), N1 != N2.

% ===========================================================================
% Spy: sees the grimoire (no mechanical effect to model here)
% ===========================================================================
first_night_substep(spy, 1, st_shows(grimoire)).

% ===========================================================================
% Spy and Recluse mis-registration
% ===========================================================================

% Spy can register as good and/or as townsfolk/outsider
% (Spy is already evil and a minion, so no misregistration needed for those)
may_mis_register(P) :- assigned(0, P, spy).
may_misregister_as(P, good) :- assigned(0, P, spy).
may_misregister_as(P, townsfolk) :- assigned(0, P, spy).
may_misregister_as(P, outsider) :- assigned(0, P, spy).

% Recluse can register as evil and/or as minion/demon
% (Recluse is already good and an outsider, so no misregistration needed for those)
may_mis_register(P) :- assigned(0, P, recluse).
may_misregister_as(P, evil) :- assigned(0, P, recluse).
may_misregister_as(P, minion) :- assigned(0, P, recluse).
may_misregister_as(P, demon) :- assigned(0, P, recluse).


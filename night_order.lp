% =============================================================================
% Blood on the Clocktower - Night Order Definitions
% =============================================================================
%
% This file defines the total ordering of roles for first night and other nights.
% Data compiled from official sources and community tools (bra1n/townsquare,
% djad99/Blood-on-the-Clocktower-Night-Order-Instructions, official script tool).
%
% night_order(Role, FirstNightOrder, OtherNightOrder)
% - FirstNightOrder: position in first night wake order (0 = doesn't wake first night)
% - OtherNightOrder: position in other nights wake order (0 = doesn't wake other nights)
%
% The numeric values represent relative ordering - lower numbers wake earlier.
% Roles with value 0 do not wake during that phase.
%
% =============================================================================

% -----------------------------------------------------------------------------
% TROUBLE BREWING
% -----------------------------------------------------------------------------

% Townsfolk
night_order(washerwoman,     33,  0).
night_order(librarian,       34,  0).
night_order(investigator,    35,  0).
night_order(chef,            36,  0).
night_order(empath,          37, 53).
night_order(fortune_teller,  38, 54).
night_order(undertaker,       0, 55).
night_order(monk,             0, 12).
night_order(ravenkeeper,      0, 52).
night_order(virgin,           0,  0).
night_order(slayer,           0,  0).
night_order(soldier,          0,  0).
night_order(mayor,            0,  0).

% Outsiders
night_order(butler,          39, 67).
night_order(drunk,            0,  0).
night_order(recluse,          0,  0).
night_order(saint,            0,  0).

% Minions
night_order(poisoner,        17,  7).
night_order(spy,             49, 68).
night_order(scarlet_woman,    0, 19).
night_order(baron,            0,  0).

% Demon
night_order(imp,              0, 24).

% Travellers (TB)
night_order(bureaucrat,       1,  1).
night_order(thief,            1,  1).
night_order(gunslinger,       0,  0).
night_order(scapegoat,        0,  0).
night_order(beggar,           0,  0).

% -----------------------------------------------------------------------------
% BAD MOON RISING
% -----------------------------------------------------------------------------

% Townsfolk
night_order(grandmother,     40, 51).
night_order(sailor,          11,  4).
night_order(chambermaid,     51, 70).
night_order(exorcist,         0, 21).
night_order(innkeeper,        0,  9).
night_order(gambler,          0, 10).
night_order(gossip,           0, 38).
night_order(courtier,        19,  8).
night_order(professor,        0, 43).
night_order(minstrel,         0,  0).
night_order(tea_lady,         0,  0).
night_order(pacifist,         0,  0).
night_order(fool,             0,  0).

% Outsiders
night_order(tinker,           0, 49).
night_order(moonchild,        0, 50).
night_order(goon,             0,  0).
night_order(lunatic,          8, 20).

% Minions
night_order(godfather,       21, 37).
night_order(devils_advocate, 22, 13).
night_order(assassin,         0, 36).
night_order(mastermind,       0,  0).

% Demons
night_order(zombuul,          0, 25).
night_order(pukka,           28, 26).
night_order(shabaloth,        0, 27).
night_order(po,               0, 28).

% Travellers (BMR)
night_order(apprentice,       1,  0).
night_order(matron,           0,  0).
night_order(judge,            0,  0).
night_order(bishop,           0,  0).
night_order(voudon,           0,  0).

% -----------------------------------------------------------------------------
% SECTS & VIOLETS
% -----------------------------------------------------------------------------

% Townsfolk
night_order(clockmaker,      41,  0).
night_order(dreamer,         42, 56).
night_order(snake_charmer,   20, 11).
night_order(mathematician,   52, 71).
night_order(flowergirl,       0, 57).
night_order(town_crier,       0, 58).
night_order(oracle,           0, 59).
night_order(savant,           0,  0).
night_order(seamstress,      43, 60).
night_order(philosopher,      2,  2).
night_order(artist,           0,  0).
night_order(juggler,          0, 61).
night_order(sage,             0, 42).

% Outsiders
night_order(mutant,           0,  0).
night_order(sweetheart,       0, 41).
night_order(barber,           0, 40).
night_order(klutz,            0,  0).

% Minions
night_order(evil_twin,       23,  0).
night_order(witch,           24, 14).
night_order(cerenovus,       25, 15).
night_order(pit_hag,          0, 16).

% Demons
night_order(fang_gu,          0, 29).
night_order(vigormortis,      0, 32).
night_order(no_dashii,        0, 30).
night_order(vortox,           0, 31).

% Travellers (S&V)
night_order(barista,          1,  1).
night_order(harlot,           0,  1).
night_order(butcher,          0,  0).
night_order(bone_collector,   0,  1).
night_order(deviant,          0,  0).

% -----------------------------------------------------------------------------
% EXPERIMENTAL / CAROUSEL
% -----------------------------------------------------------------------------

% Townsfolk
night_order(noble,           44,  0).
night_order(bounty_hunter,   46, 64).
night_order(pixie,           29,  0).
night_order(general,         50, 69).
night_order(preacher,        14,  6).
night_order(king,            10, 63).
night_order(balloonist,      45, 62).
night_order(cult_leader,     48, 66).
night_order(lycanthrope,      0, 22).
night_order(amnesiac,        32, 47).
night_order(nightwatchman,   47, 65).
night_order(engineer,        13,  5).
night_order(fisherman,        0,  0).
night_order(huntsman,        30, 45).
night_order(alchemist,        3,  0).
night_order(farmer,           0, 48).
night_order(magician,         5,  0).
night_order(choirboy,         0, 44).
night_order(poppy_grower,     4,  3).
night_order(atheist,          0,  0).
night_order(cannibal,         0,  0).
night_order(steward,         52,  0).
night_order(knight,          53,  0).
night_order(shugenja,        56,  0).
night_order(village_idiot,   57, 74).
night_order(high_priestess,  63, 81).
night_order(alsaahir,         0,  0).  % Day ability only
night_order(banshee,          0,  0).  % Triggered ability

% Outsiders
night_order(snitch,           7,  0).
night_order(acrobat,          0, 39).
night_order(puzzlemaster,     0,  0).
night_order(heretic,          0,  0).
night_order(damsel,          31, 46).
night_order(golem,            0,  0).
night_order(politician,       0,  0).
night_order(ogre,            62,  0).
night_order(plague_doctor,    0,  0).
night_order(hatter,           0,  0).
night_order(zealot,           0,  0).

% Minions
night_order(widow,           18,  0).
night_order(fearmonger,      26, 17).
night_order(psychopath,       0,  0).
night_order(goblin,           0,  0).
night_order(mephit,          27, 18).
night_order(mezepheles,      27, 18).
night_order(marionette,      12,  0).
night_order(boomdandy,        0,  0).
night_order(harpy,           35, 22).
night_order(organ_grinder,    0,  0).  % Passive ability
night_order(vizier,          69,  0).  % Announced at dawn, not a night action
night_order(summoner,        14, 25).  % Acts on night 3
night_order(wizard,          27, 12).
night_order(boffin,           5,  0).

% Demons
night_order(lil_monsta,      15, 35).
night_order(lleech,          16, 34).
night_order(al_hadikhia,      0, 33).
night_order(legion,           0, 23).
night_order(leviathan,       54, 73).
night_order(riot,             0,  0).  % Special game mode
night_order(lord_of_typhon,   3, 38).
night_order(kazali,           4, 45).
night_order(xaan,            23,  8).
night_order(yaggababble,      9, 44).
night_order(ojo,              0,  0).  % On-demand ability

% Fabled (generally don't have night actions in the traditional sense)
night_order(deus_ex_fiasco,   0,  0).
night_order(djinn,            0,  0).
night_order(angel,            0,  0).
night_order(buddhist,         0,  0).
night_order(hell_librarian,   0,  0).
night_order(revolutionary,    0,  0).
night_order(fiddler,          0,  0).
night_order(toymaker,         0,  0).
night_order(spirit_of_ivory,  0,  0).
night_order(sentinel,         0,  0).
night_order(duchess,          0,  0).
night_order(ferryman,         0,  0).
night_order(fibbin,           0,  0).
night_order(bootlegger,       0,  0).
night_order(gardener,         0,  0).
night_order(storm_catcher,    0,  0).

% Additional Travellers
night_order(gangster,         0,  0).

% =============================================================================
% SPECIAL PHASE MARKERS
% =============================================================================
% These are not roles but markers in the night order for ST reference

% night_phase_marker(dusk, 1).      % Start of night
% night_phase_marker(minion_info, 11). % Minions learn who each other are
% night_phase_marker(demon_info, 15).  % Demon learns who minions are
% night_phase_marker(dawn, 100).    % End of night

% =============================================================================
% HELPER PREDICATES
% =============================================================================

% Roles that wake on first night (have non-zero first night order)
wakes_first_night(Role) :- night_order(Role, FN, _), FN > 0.

% Roles that wake on other nights (have non-zero other night order)
wakes_other_nights(Role) :- night_order(Role, _, ON), ON > 0.

% Get first night position (for roles that wake)
first_night_position(Role, Pos) :- night_order(Role, Pos, _), Pos > 0.

% Get other nights position (for roles that wake)
other_night_position(Role, Pos) :- night_order(Role, _, Pos), Pos > 0.

% Role A acts before Role B on first night
first_night_before(A, B) :-
    night_order(A, PosA, _),
    night_order(B, PosB, _),
    PosA > 0, PosB > 0,
    PosA < PosB.

% Role A acts before Role B on other nights
other_night_before(A, B) :-
    night_order(A, _, PosA),
    night_order(B, _, PosB),
    PosA > 0, PosB > 0,
    PosA < PosB.

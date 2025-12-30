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
% IMPORTANT: All non-zero values are UNIQUE within each ordering to allow
% unambiguous ordering derivation for any script subset.
%
% =============================================================================

% -----------------------------------------------------------------------------
% TROUBLE BREWING
% -----------------------------------------------------------------------------

% Townsfolk
night_order(washerwoman,     51,  0).
night_order(librarian,       52,  0).
night_order(investigator,    53,  0).
night_order(chef,            54,  0).
night_order(empath,          55, 69).
night_order(fortune_teller,  56, 70).
night_order(undertaker,       0, 71).
night_order(monk,             0, 16).
night_order(ravenkeeper,      0, 68).
night_order(virgin,           0,  0).
night_order(slayer,           0,  0).
night_order(soldier,          0,  0).
night_order(mayor,            0,  0).

% Outsiders
night_order(butler,          57, 85).
night_order(drunk,            0,  0).
night_order(recluse,          0,  0).
night_order(saint,            0,  0).

% Minions
night_order(poisoner,        25, 10).
night_order(spy,             67, 86).
night_order(scarlet_woman,    0, 27).
night_order(baron,            0,  0).

% Demon
night_order(imp,              0, 34).

% Travellers (TB)
night_order(bureaucrat,       1,  1).
night_order(thief,            2,  2).
night_order(gunslinger,       0,  0).
night_order(scapegoat,        0,  0).
night_order(beggar,           0,  0).

% -----------------------------------------------------------------------------
% BAD MOON RISING
% -----------------------------------------------------------------------------

% Townsfolk
night_order(grandmother,     58, 67).
night_order(sailor,          17,  6).
night_order(chambermaid,     69, 88).
night_order(exorcist,         0, 29).
night_order(innkeeper,        0, 13).
night_order(gambler,          0, 14).
night_order(gossip,           0, 52).
night_order(courtier,        27, 11).
night_order(professor,        0, 57).
night_order(minstrel,         0,  0).
night_order(tea_lady,         0,  0).
night_order(pacifist,         0,  0).
night_order(fool,             0,  0).

% Outsiders
night_order(tinker,           0, 63).
night_order(moonchild,        0, 64).
night_order(goon,             0,  0).
night_order(lunatic,         14, 28).

% Minions
night_order(godfather,       29, 51).
night_order(devils_advocate, 30, 18).
night_order(assassin,         0, 50).
night_order(mastermind,       0,  0).

% Demons
night_order(zombuul,          0, 35).
night_order(pukka,           42, 36).
night_order(shabaloth,        0, 37).
night_order(po,               0, 38).

% Travellers (BMR)
night_order(apprentice,       3,  0).
night_order(matron,           0,  0).
night_order(judge,            0,  0).
night_order(bishop,           0,  0).
night_order(voudon,           0,  0).

% -----------------------------------------------------------------------------
% SECTS & VIOLETS
% -----------------------------------------------------------------------------

% Townsfolk
night_order(clockmaker,      59,  0).
night_order(dreamer,         60, 72).
night_order(snake_charmer,   28, 15).
night_order(mathematician,   70, 89).
night_order(flowergirl,       0, 73).
night_order(town_crier,       0, 74).
night_order(oracle,           0, 75).
night_order(savant,           0,  0).
night_order(seamstress,      61, 76).
night_order(philosopher,      6,  4).
night_order(artist,           0,  0).
night_order(juggler,          0, 77).
night_order(sage,             0, 56).

% Outsiders
night_order(mutant,           0,  0).
night_order(sweetheart,       0, 55).
night_order(barber,           0, 54).
night_order(klutz,            0,  0).

% Minions
night_order(evil_twin,       31,  0).
night_order(witch,           32, 19).
night_order(cerenovus,       33, 20).
night_order(pit_hag,          0, 21).

% Demons
night_order(fang_gu,          0, 39).
night_order(vigormortis,      0, 43).
night_order(no_dashii,        0, 40).
night_order(vortox,           0, 41).

% Travellers (S&V)
night_order(barista,          4,  3).
night_order(harlot,           0,  5).
night_order(butcher,          0,  0).
night_order(bone_collector,   0,  7).
night_order(deviant,          0,  0).

% -----------------------------------------------------------------------------
% EXPERIMENTAL / CAROUSEL
% -----------------------------------------------------------------------------

% Townsfolk
night_order(noble,           62,  0).
night_order(bounty_hunter,   64, 80).
night_order(pixie,           43,  0).
night_order(general,         68, 87).
night_order(preacher,        21,  8).
night_order(king,            16, 79).
night_order(balloonist,      63, 78).
night_order(cult_leader,     66, 84).
night_order(lycanthrope,      0, 30).
night_order(amnesiac,        46, 61).
night_order(nightwatchman,   65, 81).
night_order(engineer,        19,  9).
night_order(fisherman,        0,  0).
night_order(huntsman,        44, 59).
night_order(alchemist,        7,  0).
night_order(farmer,           0, 62).
night_order(magician,         9,  0).
night_order(choirboy,         0, 58).
night_order(poppy_grower,     8, 44).
night_order(atheist,          0,  0).
night_order(cannibal,         0,  0).
night_order(steward,         71,  0).
night_order(knight,          72,  0).
night_order(shugenja,        73,  0).
night_order(village_idiot,   74, 92).
night_order(high_priestess,  75, 93).
night_order(alsaahir,         0,  0).
night_order(banshee,          0,  0).

% Outsiders
night_order(snitch,          13,  0).
night_order(acrobat,          0, 53).
night_order(puzzlemaster,     0,  0).
night_order(heretic,          0,  0).
night_order(damsel,          45, 60).
night_order(golem,            0,  0).
night_order(politician,       0,  0).
night_order(ogre,            76,  0).
night_order(plague_doctor,    0,  0).
night_order(hatter,           0,  0).
night_order(zealot,           0,  0).

% Minions
night_order(widow,           26,  0).
night_order(fearmonger,      34, 22).
night_order(psychopath,       0,  0).
night_order(goblin,           0,  0).
night_order(mephit,          35, 23).
night_order(mezepheles,      36, 24).
night_order(marionette,      18,  0).
night_order(boomdandy,        0,  0).
night_order(harpy,           47, 31).
night_order(organ_grinder,    0,  0).
night_order(vizier,          77,  0).
night_order(summoner,        22, 33).
night_order(wizard,          37, 17).
night_order(boffin,          10,  0).

% Demons
night_order(lil_monsta,      23, 48).
night_order(lleech,          24, 47).
night_order(al_hadikhia,      0, 46).
night_order(legion,           0, 32).
night_order(leviathan,       78, 91).
night_order(riot,             0,  0).
night_order(lord_of_typhon,  11, 49).
night_order(kazali,          12, 66).
night_order(xaan,            38, 12).
night_order(yaggababble,     15, 65).
night_order(ojo,              0,  0).

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

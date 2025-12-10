% NRB Episode 001: A Stud In Scarlet
% Released: February 5, 2021
% Storytellers: Ben Burns and Adam
% Script: Trouble Brewing
% Result: Good team victory

#include "base.lp".
#include "../tb.lp".

#const player_count = 9.

night_number(2).
night_number(3).
day_number(2).
day_number(3).

% Seating (clockwise from wiki)
chair(luke, 0).
chair(oli, 1).
chair(blair, 2).
chair(tom, 3).
chair(elliott, 4).
chair(laurie, 5).
chair(isaac, 6).
chair(jon, 7).
chair(sullivan, 8).

% Role assignments
assigned(0, luke, imp).
assigned(0, oli, ravenkeeper).
assigned(0, blair, fortune_teller).
assigned(0, tom, monk).
assigned(0, elliott, recluse).
assigned(0, laurie, scarlet_woman).
assigned(0, isaac, undertaker).
assigned(0, jon, chef).
assigned(0, sullivan, drunk).  % believed he was empath

% ===========================================================================
% Night 1
% ===========================================================================

% Chef (jon) learns 1 - because recluse registered as evil next to scarlet woman
:- not st_tells(chef, jon, count(1), night(1, 5, 1)).

% Sullivan (drunk, thinks he's empath) learns 0 - ST can tell him anything
st_tells(empath, sullivan, count(0), night(1, 6, 1)).

% Fortune Teller's red herring is Sullivan
:- not reminder_on(ft_red_herring, sullivan, night(1, 0, 0)).

% Fortune Teller (Blair) picks Tom and Elliott on night 1, gets "yes"
player_chooses(fortune_teller, blair, point(tom), night(1, 7, 2)).
player_chooses(fortune_teller, blair, point(elliott), night(1, 7, 2)).
:- not st_tells(fortune_teller, blair, yes, night(1, 7, 3)).

% TODO: more night 1 actions (monk, etc.)

% ===========================================================================
% Day 1: no execution
% ===========================================================================

% ===========================================================================
% Night 2
% ===========================================================================

% Monk (Tom) protects Sullivan
player_chooses(monk, tom, point(sullivan), night(2, 2, 2)).

% Imp (Luke) targets Sullivan - but he's protected!
player_chooses(imp, luke, point(sullivan), night(2, 4, 2)).

% Sullivan (drunk, thinks he's empath) learns 0
st_tells(empath, sullivan, count(0), night(2, 6, 1)).

% Fortune Teller (Blair) picks Tom and Oli
player_chooses(fortune_teller, blair, point(tom), night(2, 7, 2)).
player_chooses(fortune_teller, blair, point(oli), night(2, 7, 2)).

% ===========================================================================
% Day 2: jon executed
% ===========================================================================

executed(jon, 2).

% ===========================================================================
% Night 3
% ===========================================================================

% Monk (Tom) protects Sullivan
player_chooses(monk, tom, point(sullivan), night(3, 2, 2)).

% Imp (Luke) targets Tom - Tom dies
player_chooses(imp, luke, point(tom), night(3, 4, 2)).

% Undertaker (Isaac) learns Jon was Chef
% (This should be derived automatically from executed(jon, 2))

% Sullivan (drunk, thinks he's empath) learns 1
st_tells(empath, sullivan, count(1), night(3, 6, 1)).

% Fortune Teller (Blair) picks Laurie and Isaac
% (Blair must get "no" - proved by contradiction: asserting "yes" is UNSAT)
player_chooses(fortune_teller, blair, point(laurie), night(3, 7, 2)).
player_chooses(fortune_teller, blair, point(isaac), night(3, 7, 2)).

% ===========================================================================
% Day 3: Tom announced dead, Elliott executed
% ===========================================================================

% Tom was killed by imp on Night 3 (already encoded above)
executed(elliott, 3).

% ===========================================================================
% Night 4
% ===========================================================================

night_number(4).
day_number(4).

% Imp (Luke) targets Oli
player_chooses(imp, luke, point(oli), night(4, 4, 2)).

% Ravenkeeper (Oli) targets Laurie (Oli is dying, so ability triggers)
player_chooses(ravenkeeper, oli, point(laurie), night(4, 5, 2)).

% Sullivan (drunk, thinks he's empath) learns 0
st_tells(empath, sullivan, count(0), night(4, 6, 1)).

% Fortune Teller (Blair) picks Sullivan and Luke
player_chooses(fortune_teller, blair, point(sullivan), night(4, 7, 2)).
player_chooses(fortune_teller, blair, point(luke), night(4, 7, 2)).

% Undertaker (Isaac) learns Elliott was "poisoner" (Recluse mis-registration)
:- not st_tells(undertaker, isaac, point(poisoner), night(4, 8, 1)).

% ===========================================================================
% Day 4: Oli announced dead, Luke executed
% ===========================================================================

% Oli was killed by imp on Night 4 (already encoded above)
executed(luke, 4).

% Scarlet Woman triggers: Laurie becomes the new Imp
% (5+ players alive when demon dies by execution)

% ===========================================================================
% Night 5
% ===========================================================================

night_number(5).
day_number(5).

% Laurie (now the Imp) targets Sullivan
player_chooses(imp, laurie, point(sullivan), night(5, 4, 2)).

% Fortune Teller (Blair) picks Laurie and Isaac
player_chooses(fortune_teller, blair, point(laurie), night(5, 7, 2)).
player_chooses(fortune_teller, blair, point(isaac), night(5, 7, 2)).

% ===========================================================================
% Day 5: Sullivan announced dead, Laurie executed (good wins)
% ===========================================================================

executed(laurie, 5).

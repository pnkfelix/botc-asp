% Test: Night 2 - Empath skips dead neighbors to find living neighbors
#include "base.lp".

needs_night(2).

% Seating: amanda - rob - taylor - courtney - steph - felix - neha - pratik
% amanda is empath
% rob (good) is her clockwise neighbor - will be killed
% taylor (evil) is next clockwise - will become living neighbor after rob dies
% pratik (good) is counter-clockwise neighbor

assigned(0, amanda, empath).
assigned(0, rob, washerwoman).       % good, amanda's immediate clockwise neighbor
assigned(0, taylor, poisoner).       % evil - will become living neighbor after rob dies
assigned(0, courtney, butler).       % outsider (8 players need 1 outsider)
assigned(0, steph, imp).
assigned(0, felix, chef).
assigned(0, neha, mayor).
assigned(0, pratik, slayer).         % good, amanda's immediate counter-clockwise neighbor

% Ensure empath is not poisoned
:- reminder_on(poi_poisoned, amanda, _).

% Night 2: Imp kills rob (amanda's immediate neighbor)
player_chooses(imp, steph, point(rob), night(2, 4, 2)).

% On night 1, amanda's living neighbors are rob (good) and pratik (good) = 0 evil
% On night 2, after imp acts, rob is dead (has imp_dead token)
% Empath acts AFTER imp in other_night_order (empath=6, imp=4)
% Amanda's living neighbors become: taylor (evil) and pratik (good) = 1 evil

% Assert: on night 2, empath should be told 1 (taylor=evil, pratik=good)
:- not st_tells_core(empath, amanda, count(1), night(2, 6, 1)).

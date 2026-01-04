% Test: Empath killed by Imp same night - ability does NOT fire
% If the Empath is killed by the Imp during the night, their ability
% does not fire because they are dead at the time they would wake.
%
% Night order: Imp (position 4) acts before Empath (position 6)
% When Imp kills Empath at night(N, 4, 3), Empath is dead by night(N, 6, 1)
%
% This is SAT because the empath correctly receives NO info when dead.
#include "base.lp".

needs_night(2).

% Seating: amanda(empath) - rob - taylor - courtney - steph - felix - neha - pratik
assigned(0, amanda, empath).
assigned(0, rob, washerwoman).
assigned(0, taylor, poisoner).      % evil
assigned(0, courtney, butler).      % outsider (8 players need 1 outsider)
assigned(0, steph, imp).
assigned(0, felix, chef).
assigned(0, neha, mayor).
assigned(0, pratik, slayer).

% Ensure empath is not poisoned (so ability would function if she were alive)
:- reminder_on(poi_poisoned, amanda, _).

% Ensure empath is not protected by monk
:- reminder_on(monk_protected, amanda, _).

% Night 2: Imp kills amanda (the empath herself)
player_chooses(imp, steph, point(amanda), night(2, 4, 2)).

% Assert: empath should NOT get any info on night 2 (because she's dead)
% This constraint forbids any st_tells_core for empath on night 2.
% The test is SAT if the system correctly withholds info from dead empath.
:- st_tells_core(empath, amanda, _, night(2, 6, 1)).

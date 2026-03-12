% Minimal distribution counter for cross-validation with botc-zdd-
% This only counts valid role SETS (which roles are in play),
% ignoring seat assignments, night actions, etc.

% Include script definitions for role categories
% (we inline TB roles rather than including tb.lp which pulls in all game logic)

% Trouble Brewing roles
townsfolk(washerwoman; librarian; investigator; chef; empath; fortune_teller; undertaker; monk; ravenkeeper; virgin; slayer; soldier; mayor).
outsider(butler; drunk; recluse; saint).
minion(poisoner; spy; scarlet_woman; baron).
demon(imp).

role(X) :- townsfolk(X).
role(X) :- outsider(X).
role(X) :- minion(X).
role(X) :- demon(X).

% Baron's outsider adjustment
causes_outsider_mod(baron, 1) :- player_count < 7.
causes_outsider_mod(baron, 2) :- player_count >= 7.
causes_townsfolk_mod(R, -N) :- causes_outsider_mod(R, N).

% Base distribution counts (from botc.lp)
base_townsfolk(3) :- 5 <= player_count, player_count <= 6.
base_townsfolk(5) :- 7 <= player_count, player_count <= 9.
base_townsfolk(7) :- 10 <= player_count, player_count <= 12.
base_townsfolk(9) :- 13 <= player_count, player_count <= 15.

base_outsider(player_count - 5) :- 5 <= player_count, player_count <= 6.
base_outsider(player_count - 7) :- 7 <= player_count, player_count <= 9.
base_outsider(player_count - 10) :- 10 <= player_count, player_count <= 12.
base_outsider(player_count - 13) :- 13 <= player_count, player_count <= 15.

base_minion(1) :- 5 <= player_count, player_count <= 9.
base_minion(2) :- 10 <= player_count, player_count <= 12.
base_minion(3) :- 13 <= player_count, player_count <= 15.

base_demon(1).

% Choose a distribution: select roles to be "in play"
{ distrib(X) : townsfolk(X) }.
{ distrib(X) : outsider(X) }.
{ distrib(X) : minion(X) }.
{ distrib(X) : demon(X) }.

% Outsider adjustment (only Baron in TB)
outsider_adjustment(A) :- A = #sum { N, R : causes_outsider_mod(R, N), distrib(R) }.
townsfolk_adjustment(A) :- A = #sum { N, R : causes_townsfolk_mod(R, N), distrib(R) }.

% No minion/demon adjustments in TB, but define them for completeness
causes_minion_mod(none, 0) :- #false.
causes_demon_mod(none, 0) :- #false.
minion_adjustment(A) :- A = #sum { N, R : causes_minion_mod(R, N), distrib(R) }.
demon_adjustment(A) :- A = #sum { N, R : causes_demon_mod(R, N), distrib(R) }.

adjusted_townsfolk(B + A) :- base_townsfolk(B), townsfolk_adjustment(A).
adjusted_outsider(B + A) :- base_outsider(B), outsider_adjustment(A).
adjusted_minion(B + A) :- base_minion(B), minion_adjustment(A).
adjusted_demon(B + A) :- base_demon(B), demon_adjustment(A).

% Enforce correct counts
:- adjusted_townsfolk(N), #count { R : distrib(R), townsfolk(R) } != N.
:- adjusted_outsider(N), #count { R : distrib(R), outsider(R) } != N.
:- adjusted_minion(N), #count { R : distrib(R), minion(R) } != N.
:- adjusted_demon(N), #count { R : distrib(R), demon(R) } != N.

#show distrib/1.

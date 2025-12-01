% night+day are represented by time steps
% 0 is night 1, 1 is day 1, 2 is night 2, 3 is day 2, etc
time(0..max_time).

% in principle we may need this to be O(#players), e.g. 30 or so,
% but for now we leave it "small"
#const max_time = 10.

player(P) :- name(P), chair(P,C), C < player_count.

% outsider_adjustment(A) :- A = #sum { N, R: causes_outsider_mod(R, N), distrib(R) }.
outsider_adjustment(A) :- A = #sum { N, R: causes_outsider_mod(R, N), assigned(0, _, R) }.

minion_adjustment(A) :- A = #sum { N, R: causes_minion_mod(R, N), assigned(0, _, R) }.
demon_adjustment(A) :- A = #sum { N, R: causes_demon_mod(R, N), assigned(0, _, R) }.

causes_townsfolk_mod(R, -N) :- causes_outsider_mod(R, N).

causes_minion_mod(none, 0) :- #false.
causes_demon_mod(none, 0) :- #false.

townsfolk_adjustment(A) :- A = #sum { N, R: causes_townsfolk_mod(R, N), assigned(0, _, R) }.

adjusted_townsfolk(B + A) :- base_townsfolk(B), townsfolk_adjustment(A).
adjusted_outsider(B + A) :- base_outsider(B), outsider_adjustment(A).
adjusted_minion(B + A) :- base_minion(B), minion_adjustment(A).
adjusted_demon(B + A) :- base_demon(B), demon_adjustment(A).

role(X) :- townsfolk(X).
role(X) :- outsider(X).
role(X) :- minion(X).
role(X) :- demon(X).

reminder(poi_poisoned).

% the legal baseline role distrbutions are:
% 7 players => 1 demon, 1 minion, 0 outsiders, 5 townsfolk

base_townsfolk(5) :- 7 <= player_count <= 9.
base_townsfolk(7) :- 10 <= player_count <= 12.
base_townsfolk(9) :- 13 <= player_count <= 15.

base_outsider(player_count - 7) :- 7 <= player_count <= 9.
base_outsider(player_count - 10) :- 10 <= player_count <= 12.
base_outsider(player_count - 13) :- 13 <= player_count <= 15.

base_minion(1) :-  7 <= player_count <= 9.
base_minion(2) :- 10 <= player_count <= 12.
base_minion(3) :- 13 <= player_count <= 15.

base_demon(1).

distrib(R) :- assigned(0, _, R).
%{distrib(X) : demon(X)} = 1.
%{distrib(X) : minion(X)} = 1    :- 7 <= player_count <= 9.
%{distrib(X) : townsfolk(X)} = 5 - A :- 7 <= player_count <= 9, outsider_adjustment(A).
%{distrib(X) : outsider(X)} = player_count - 7 + A :- 7 <= player_count <= 9, outsider_adjustment(A).
%
%{distrib(X) : minion(X)} = 2    :- 10 <= player_count <= 12.
%{distrib(X) : townsfolk(X)} = 7 - A :- 10 <= player_count <= 12, outsider_adjustment(A).
%{distrib(X) : outsider(X)} = player_count - 10 + A :- 10 <= player_count <= 12, outsider_adjustment(A).
%
%{distrib(X) : minion(X)} = 3    :- 13 <= player_count <= 15.
%{distrib(X) : townsfolk(X)} = 9 :- 13 <= player_count <= 15.
%{distrib(X) : outsider(X)} = player_count - 13 + A :- 13 <= player_count <= 15, outsider_adjustment(A).

% belt+suspenders: the number of distributed roles must equal player count
%:- not {distrib(X)} = player_count.

% most roles can be in the bag (except things like drunk, marionette).
{bag(X) : role(X), not never_in_bag(X)} = player_count.

% each player receives exactly one token from the bag.
{received(P, X) : bag(X)} = 1 :- player(P).
% a token is not received by multiple players
:- received(P1, X), received(P2, X), P1 != P2.

% each player is assigned exactly one unique role
{ assigned(0, P, R) : role(R) } = 1 :- player(P).
R1 != R2 :- assigned(0, P1, R1), assigned(0, P2, R2), P1 != P2.

%{assigned(0, P, X) : player(P), distrib(X)} = player_count.
%R1 = R2 :- assigned(0, P, R1), assigned(0, P, R2).
%R1 != R2 :- assigned(0, P1, R1), assigned(0, P2, R2), P1 != P2.

thinks_it_is(X, X) :- role(X), not mistaken_identity(X, _).
thinks_it_is(R, Y) :- mistaken_identity(R, townsfolk), townsfolk(Y).
thinks_it_is(R, Y) :- mistaken_identity(R, demon), demon(Y).
thinks_it_is(R, Y) :- mistaken_identity(R, minion), minion(Y).

% the role assigned is one that "thinks it is the" token received.
:- received(P, T), assigned(_, P, R), not thinks_it_is(R, T).

:- adjusted_townsfolk(N), #count { R : assigned(0, _, R), townsfolk(R) } != N.
:- adjusted_outsider(N), #count { R : assigned(0, _, R), outsider(R) } != N.
:- adjusted_minion(N), #count { R : assigned(0, _, R), minion(R) } != N.
:- adjusted_demon(N), #count { R : assigned(0, _, R), demon(R) } != N.

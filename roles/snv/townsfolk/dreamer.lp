% Dreamer
% Each night, choose a player (not yourself): you learn 1 good and 1 evil character, 1 of which is correct.

first_night_substep(dreamer, 1, st_asks(choose_target)).
first_night_substep(dreamer, 2, player_chooses(target)).
first_night_substep(dreamer, 3, st_tells(dreamer_info)).

other_night_substep(dreamer, 1, st_asks(choose_target)).
other_night_substep(dreamer, 2, player_chooses(target)).
other_night_substep(dreamer, 3, st_tells(dreamer_info)).

% Night 1 choice: Dreamer picks a player (can't merge with other nights: uses first_night_role_order, different substep registration)
{ player_chooses(dreamer, Dreamer, point(P), night(1, RoleOrd, 2)) : player(P), P != Dreamer } = 1 :-
    first_night_role_order(dreamer, RoleOrd),
    assigned(0, Dreamer, dreamer).

% Other nights choice: Dreamer picks a player (can't merge with night 1: uses other_night_role_order, requires alive check)
{ player_chooses(dreamer, Dreamer, point(P), night(N, RoleOrd, 2)) : player(P), P != Dreamer } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(dreamer, RoleOrd),
    assigned(0, Dreamer, dreamer),
    alive(Dreamer, night(N, 0, 0)).

% Helper: good characters are townsfolk or outsiders (two rules, one per category; can't merge: separate base predicates)
good_character(R) :- townsfolk(R).
good_character(R) :- outsider(R).

% Helper: evil characters are minions or demons (two rules, one per category; can't merge: separate base predicates)
evil_character(R) :- minion(R).
evil_character(R) :- demon(R).

% Functioning info choice: ST picks one good and one evil character to show (can't merge with not-functioning: correctness constraint below applies only here)
{ dreamer_info(Dreamer, Target, GoodChar, EvilChar, T) :
    good_character(GoodChar), evil_character(EvilChar) } = 1 :-
    acting_role(T, dreamer),
    assigned(0, Dreamer, dreamer),
    player_chooses(dreamer, Dreamer, point(Target), T2),
    T = night(N, RoleOrd, 3),
    T2 = night(N, RoleOrd, 2),
    alive(Dreamer, night(N, 0, 0)),
    functioning(Dreamer, T).

% Correctness constraint: target's actual role must match one of the two shown (can't fold into choice rule: ASP requires separate integrity constraint)
:- dreamer_info(Dreamer, Target, GoodChar, EvilChar, T),
   functioning(Dreamer, T),
   assigned(0, Target, ActualRole),
   ActualRole != GoodChar,
   ActualRole != EvilChar.

% Not-functioning info choice: ST freely picks any good/evil pair (can't merge with functioning: no correctness constraint applies)
{ dreamer_info(Dreamer, Target, GoodChar, EvilChar, T) :
    good_character(GoodChar), evil_character(EvilChar) } = 1 :-
    acting_role(T, dreamer),
    assigned(0, Dreamer, dreamer),
    player_chooses(dreamer, Dreamer, point(Target), T2),
    T = night(N, RoleOrd, 3),
    T2 = night(N, RoleOrd, 2),
    alive(Dreamer, night(N, 0, 0)),
    not functioning(Dreamer, T).

% Bridge to st_tells_core: converts dreamer_info into the standard tell format (can't inline: dreamer_info carries Target for correctness constraint)
st_tells_core(dreamer, Dreamer, info(GoodChar, EvilChar), T) :-
    dreamer_info(Dreamer, _, GoodChar, EvilChar, T).

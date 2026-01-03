% Dreamer
% Each night, choose a player (not yourself): you learn 1 good and 1 evil character, 1 of which is correct.

first_night_substep(dreamer, 1, st_asks(choose_target)).
first_night_substep(dreamer, 2, player_chooses(target)).
first_night_substep(dreamer, 3, st_tells(dreamer_info)).

other_night_substep(dreamer, 1, st_asks(choose_target)).
other_night_substep(dreamer, 2, player_chooses(target)).
other_night_substep(dreamer, 3, st_tells(dreamer_info)).

% Dreamer chooses a player on first night
{ player_chooses(dreamer, Dreamer, point(P), night(1, RoleOrd, 2)) : player(P), P != Dreamer } = 1 :-
    first_night_role_order(dreamer, RoleOrd),
    assigned(0, Dreamer, dreamer).

% Dreamer chooses a player on other nights (if alive)
{ player_chooses(dreamer, Dreamer, point(P), night(N, RoleOrd, 2)) : player(P), P != Dreamer } = 1 :-
    night_number(N), N > 1,
    other_night_role_order(dreamer, RoleOrd),
    assigned(0, Dreamer, dreamer),
    alive(Dreamer, night(N, 0, 0)).

% Good character options (townsfolk or outsider)
good_character(R) :- townsfolk(R).
good_character(R) :- outsider(R).

% Evil character options (minion or demon)
evil_character(R) :- minion(R).
evil_character(R) :- demon(R).

% When functioning: one of the two characters shown must be correct
% The Dreamer sees the target's role as one of the two shown
{ dreamer_info(Dreamer, Target, GoodChar, EvilChar, T) :
    good_character(GoodChar), evil_character(EvilChar) } = 1 :-
    acting_role(T, dreamer),
    assigned(0, Dreamer, dreamer),
    player_chooses(dreamer, Dreamer, point(Target), T2),
    T = night(N, RoleOrd, 3),
    T2 = night(N, RoleOrd, 2),
    alive(Dreamer, night(N, 0, 0)),
    functioning(Dreamer, T).

% Constraint: the correct role must be one of the two shown (when functioning)
:- dreamer_info(Dreamer, Target, GoodChar, EvilChar, T),
   functioning(Dreamer, T),
   assigned(0, Target, ActualRole),
   ActualRole != GoodChar,
   ActualRole != EvilChar.

% When not functioning: any good and evil character
{ dreamer_info(Dreamer, Target, GoodChar, EvilChar, T) :
    good_character(GoodChar), evil_character(EvilChar) } = 1 :-
    acting_role(T, dreamer),
    assigned(0, Dreamer, dreamer),
    player_chooses(dreamer, Dreamer, point(Target), T2),
    T = night(N, RoleOrd, 3),
    T2 = night(N, RoleOrd, 2),
    alive(Dreamer, night(N, 0, 0)),
    not functioning(Dreamer, T).

% Convert to st_tells_core
st_tells_core(dreamer, Dreamer, info(GoodChar, EvilChar), T) :-
    dreamer_info(Dreamer, _, GoodChar, EvilChar, T).

% Philosopher
% Once per game, at night, choose a good character: gain that ability. If this character is in play, they are drunk.

first_night_substep(philosopher, 1, st_asks(choose_character)).
first_night_substep(philosopher, 2, player_chooses(good_character)).
first_night_substep(philosopher, 3, st_places(philosopher_drunk)).

other_night_substep(philosopher, 1, st_asks(choose_character)).
other_night_substep(philosopher, 2, player_chooses(good_character)).
other_night_substep(philosopher, 3, st_places(philosopher_drunk)).

% Track when philosopher uses their ability
philosopher_used(Philosopher, N) :-
    philosopher_chooses(Philosopher, _, night(N, _, _)).

% Philosopher may choose a good character on any night (once per game)
{ philosopher_chooses(Philosopher, Role, night(1, RoleOrd, 2)) : good_character(Role) } <= 1 :-
    first_night_role_order(philosopher, RoleOrd),
    assigned(0, Philosopher, philosopher),
    alive(Philosopher, night(1, 0, 0)).

{ philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)) : good_character(Role) } <= 1 :-
    other_night_role_order(philosopher, RoleOrd),
    night_number(N), N > 1,
    assigned(0, Philosopher, philosopher),
    alive(Philosopher, night(N, 0, 0)),
    not philosopher_used(Philosopher, PrevN) : night_number(PrevN), PrevN < N.

% Cannot use ability twice
:- philosopher_used(Philosopher, N1), philosopher_used(Philosopher, N2), N1 != N2.

% Convert to player_chooses
player_chooses(philosopher, Philosopher, point(Role), T) :-
    philosopher_chooses(Philosopher, Role, T).

% If the chosen character is in play, they become drunk
% The "phil_drunk" token causes impairment
reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = night(N2, _, _), N2 >= N.

reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = day(N2, _), N2 >= N.

reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = dawn(N2), N2 >= N.

causes_impairment(phil_drunk).

% The Philosopher gains the chosen ability
% This is complex to model fully - for now, we mark that they have the ability
philosopher_has_ability(Philosopher, Role, N) :-
    philosopher_chooses(Philosopher, Role, night(N, _, _)),
    functioning(Philosopher, night(N, _, 3)).

% Note: Full implementation would need to duplicate all role rules with philosopher
% as the acting player. This is a framework for that functionality.

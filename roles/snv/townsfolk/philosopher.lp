% Philosopher
% Once per game, at night, choose a good character: gain that ability. If this character is in play, they are drunk.

first_night_substep(philosopher, 1, st_asks(choose_character)).
first_night_substep(philosopher, 2, player_chooses(good_character)).
first_night_substep(philosopher, 3, st_places(philosopher_drunk)).

other_night_substep(philosopher, 1, st_asks(choose_character)).
other_night_substep(philosopher, 2, player_chooses(good_character)).
other_night_substep(philosopher, 3, st_places(philosopher_drunk)).

% Helper: track when ability was used (can't inline: referenced by both choice rules and integrity constraint)
philosopher_used(Philosopher, N) :-
    philosopher_chooses(Philosopher, _, night(N, _, _)).

% Night 1 choice: may choose a good character (can't merge with other nights: different role order, no once-per-game check)
{ philosopher_chooses(Philosopher, Role, night(1, RoleOrd, 2)) : good_character(Role) } <= 1 :-
    first_night_role_order(philosopher, RoleOrd),
    assigned(0, Philosopher, philosopher),
    alive(Philosopher, night(1, 0, 0)).

% Other nights choice: adds once-per-game guard via negation (separate from night 1: uses other_night_role_order, conditional negation)
{ philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)) : good_character(Role) } <= 1 :-
    other_night_role_order(philosopher, RoleOrd),
    night_number(N), N > 1,
    assigned(0, Philosopher, philosopher),
    alive(Philosopher, night(N, 0, 0)),
    not philosopher_used(Philosopher, PrevN) : night_number(PrevN), PrevN < N.

% Integrity constraint: once-per-game enforcement (can't fold into choice rule: provides global cross-night guarantee)
:- philosopher_used(Philosopher, N1), philosopher_used(Philosopher, N2), N1 != N2.

% Bridge to player_chooses interface (deterministic projection; separate from choice rules: different predicate)
player_chooses(philosopher, Philosopher, point(Role), T) :-
    philosopher_chooses(Philosopher, Role, T).

% Drunk token on night time points (can't merge with day/dawn: different time type in unification)
reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = night(N2, _, _), N2 >= N.

% Drunk token on day time points (can't merge with night/dawn: day/2 vs night/3 vs dawn/1 arity)
reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = day(N2, _), N2 >= N.

% Drunk token on dawn time points (can't merge with night/day: dawn/1 has different arity)
reminder_on(phil_drunk, P, T) :-
    philosopher_chooses(Philosopher, Role, night(N, RoleOrd, 2)),
    functioning(Philosopher, night(N, RoleOrd, 3)),
    assigned(0, P, Role),
    P != Philosopher,
    time(T), T = dawn(N2), N2 >= N.

% Declare phil_drunk as an impairment source (fact, not a rule; separate from reminder_on derivations)
causes_impairment(phil_drunk).

% Derive gained ability when functioning (separate from choice/drunk rules: models ability gain, not token placement)
philosopher_has_ability(Philosopher, Role, N) :-
    philosopher_chooses(Philosopher, Role, night(N, _, _)),
    functioning(Philosopher, night(N, _, 3)).

% Note: Full implementation would need to duplicate all role rules with philosopher
% as the acting player. This is a framework for that functionality.

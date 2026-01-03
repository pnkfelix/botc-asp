% Fool
% The first time you die, you don't.

% Track when Fool would die
fool_would_die(Fool, T) :-
    assigned(0, Fool, fool),
    would_die(Fool, T).

% General "would die" predicate for various death causes
would_die(P, T) :- imp_kill_succeeds(P, T), not protected_from_demon(P, T).
would_die(P, T) :- zombuul_kill_succeeds(P, T), not protected_from_demon(P, T).
would_die(P, T) :- pukka_kill_succeeds(P, T), not protected_from_demon(P, T).
would_die(P, T) :- shabaloth_kill_succeeds(P, T), not protected_from_demon(P, T).
would_die(P, T) :- po_kill_succeeds(P, T), not protected_from_demon(P, T).
would_die(P, T) :- executed(P, N), T = day(N, exec).

% First death attempt on Fool
fool_first_death(Fool, T) :-
    fool_would_die(Fool, T),
    not fool_died_before(Fool, T).

% Fool died at some earlier time
fool_died_before(Fool, T2) :-
    fool_would_die(Fool, T1),
    time(T1), time(T2),
    next_transitive(T1, T2).

% Transitive next (T1 comes before T2)
next_transitive(T1, T2) :- next(T1, T2).
next_transitive(T1, T3) :- next(T1, T2), next_transitive(T2, T3).

% Fool survives first death (if functioning)
fool_survives(Fool, T) :-
    fool_first_death(Fool, T),
    functioning(Fool, T).

% Place reminder when Fool uses their ability
reminder_on(fool_used, Fool, T) :-
    fool_survives(Fool, T).

% Token persists
reminder_on(fool_used, Fool, T2) :-
    reminder_on(fool_used, Fool, T1),
    next(T1, T2).

% Prevent Fool's first death when functioning
:- fool_survives(Fool, T), died(Fool, T).

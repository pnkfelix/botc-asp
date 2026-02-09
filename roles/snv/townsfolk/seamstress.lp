% Seamstress
% Once per game, at night, choose 2 players (not yourself): you learn if they are the same alignment.

first_night_substep(seamstress, 1, st_asks(choose_two)).
first_night_substep(seamstress, 2, player_chooses(two_players)).
first_night_substep(seamstress, 3, st_tells(seamstress_info)).

other_night_substep(seamstress, 1, st_asks(choose_two)).
other_night_substep(seamstress, 2, player_chooses(two_players)).
other_night_substep(seamstress, 3, st_tells(seamstress_info)).

% Helper: true when this Seamstress made a choice on night N.
% Factored out so the once-per-game guard and uniqueness constraint can reference it.
seamstress_used(Seamstress, N) :-
    seamstress_chooses(Seamstress, _, _, night(N, _, _)).

% Night 1 choice: Seamstress may choose two players (optional, <= 1).
% Separate from other-nights: uses first_night_role_order and has no once-per-game guard.
{ seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)) :
    player(P1), player(P2), P1 != P2, P1 != Seamstress, P2 != Seamstress,
    chair(P1, C1), chair(P2, C2), C1 < C2 } <= 1 :-
    first_night_role_order(seamstress, RoleOrd),
    N = 1,
    assigned(0, Seamstress, seamstress),
    alive(Seamstress, night(N, 0, 0)).

% Other-nights choice: Seamstress may choose two players if ability not yet used.
% Separate from night 1: uses other_night_role_order, requires alive check, and enforces once-per-game.
{ seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)) :
    player(P1), player(P2), P1 != P2, P1 != Seamstress, P2 != Seamstress,
    chair(P1, C1), chair(P2, C2), C1 < C2 } <= 1 :-
    other_night_role_order(seamstress, RoleOrd),
    night_number(N), N > 1,
    assigned(0, Seamstress, seamstress),
    alive(Seamstress, night(N, 0, 0)),
    not seamstress_used(Seamstress, PrevN) : night_number(PrevN), PrevN < N.

% Uniqueness constraint: ability can only be used once per game.
% Integrity constraint supplements the conditional guard in the choice rules above.
:- seamstress_used(Seamstress, N1), seamstress_used(Seamstress, N2), N1 != N2.

% Bridge to player_chooses: emit one fact per chosen player for the generic framework.
% Two rules needed because seamstress_chooses packs both players into one tuple.
player_chooses(seamstress, Seamstress, point(P1), T) :-
    seamstress_chooses(Seamstress, P1, _, T).
player_chooses(seamstress, Seamstress, point(P2), T) :-
    seamstress_chooses(Seamstress, _, P2, T).

% Same-alignment check, good/good case: both players register as good.
% Two rules needed because ASP has no disjunction in rule bodies; each alignment is a separate case.
same_alignment(P1, P2, T) :-
    player(P1), player(P2), time(T),
    registers_as(P1, good, T),
    registers_as(P2, good, T).
% Same-alignment check, evil/evil case: both players register as evil.
same_alignment(P1, P2, T) :-
    player(P1), player(P2), time(T),
    registers_as(P1, evil, T),
    registers_as(P2, evil, T).

% Functioning 'yes' tell: deterministic when chosen players share alignment.
% Can't merge with 'no' rule: opposite condition (same_alignment vs not same_alignment).
st_tells_core(seamstress, Seamstress, yes, T) :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    functioning(Seamstress, T),
    same_alignment(P1, P2, T).

% Functioning 'no' tell: deterministic when chosen players differ in alignment.
% Can't merge with 'yes' rule: negated condition requires a separate rule in ASP.
st_tells_core(seamstress, Seamstress, no, T) :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, P1, P2, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    functioning(Seamstress, T),
    not same_alignment(P1, P2, T).

% Not-functioning tell: ST freely chooses yes or no (choice rule).
% Can't merge with functioning rules: different semantics (choice vs deterministic).
{ st_tells_core(seamstress, Seamstress, yes, T);
  st_tells_core(seamstress, Seamstress, no, T) } = 1 :-
    T = night(N, RoleOrd, 3),
    seamstress_chooses(Seamstress, _, _, night(N, RoleOrd, 2)),
    assigned(0, Seamstress, seamstress),
    not functioning(Seamstress, T).

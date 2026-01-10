% Slayer

% Anyone who received slayer token can attempt a shot (includes drunk/marionette)
:- slayer_shot(Slayer, _, _), not received(Slayer, slayer).
:- slayer_shot(Slayer, _, N), not alive(Slayer, day(N, 0)).
:- slayer_shot(_, Target, N), not alive(Target, day(N, 0)).

% Token is placed for anyone who shoots (tracks the shot attempt)
reminder_on(sla_no_ability, Slayer, day(N, 0)) :- slayer_shot(Slayer, _, N).

:- slayer_shot(Slayer, _, N),
   reminder_on(sla_no_ability, Slayer, T),
   T = day(N2, _), N2 < N.

% Kill only happens if shooter is actual slayer and functioning
died(Target, day(N, 0)) :-
    slayer_shot(Slayer, Target, N),
    assigned(0, Slayer, slayer),
    functioning(Slayer, day(N, 0)),
    registers_as(Target, demon, day(N, 0)).

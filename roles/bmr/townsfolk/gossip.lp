% Gossip
% Each day, you may make a public statement. Tonight, if it was true, a player dies.

other_night_substep(gossip, 1, st_places(gossip_dead)).

% ST chooses whether the Gossip's public statement was true (abstract: statement content not modeled)
{ gossip_statement_true(Gossip, N) } :-
    day_number(N),
    assigned(0, Gossip, gossip),
    alive(Gossip, day(N, 0)).

% If Gossip is functioning and statement was true, ST chooses one alive player to die tonight
{ gossip_kills(P, N) : player(P), P != Gossip, alive(P, night(N+1, 0, 0)) } = 1 :-
    day_number(N),
    night_number(N+1),
    assigned(0, Gossip, gossip),
    gossip_statement_true(Gossip, N),
    functioning(Gossip, day(N, exec)).

% Constraint: no kill if Gossip was not functioning (can't merge with choice rule: negation of functioning)
:- gossip_kills(_, N),
   assigned(0, Gossip, gossip),
   gossip_statement_true(Gossip, N),
   not functioning(Gossip, day(N, exec)).

% Place death token on victim for grimoire display (kill resolves at start of next night)
reminder_on(gossip_dead, P, night(N+1, RoleOrd, 1)) :-
    other_night_role_order(gossip, RoleOrd),
    gossip_kills(P, N).

% Mark victim as dead at the instant of the kill (for mid-night state tracking)
dead_at_time(P, T) :- reminder_on(gossip_dead, P, T).
% Record death at final night time for dawn processing (separate: different timing semantics)
died(P, T) :- gossip_kills(P, N), final_night_time(N+1, T), alive(P, night(N+1, 0, 0)).

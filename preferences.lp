% ===========================================================================
% Storyteller Preferences (Weak Constraints)
% ===========================================================================
%
% These are "soft constraints" that discourage certain game states without
% forbidding them. While technically legal by the rules, these situations
% would rarely happen in an actual game because a skilled Storyteller would
% avoid them unless necessary.
%
% Clingo will prefer answer sets with lower total penalty, but will still
% consider penalized solutions when no unpenalized ones exist.
%
% Syntax: :~ condition. [weight@priority, terms]
%   - weight: penalty added for each grounding of terms matching condition
%   - priority: higher priorities are minimized first (lexicographic ordering)
%   - terms: variables to ground over (for counting distinct violations)
%
% ===========================================================================

% ---------------------------------------------------------------------------
% Priority 1: Self-pings (information roles seeing themselves)
% ---------------------------------------------------------------------------
% A Washerwoman/Librarian/Investigator seeing themselves as one of their
% two pings is legal, but a Storyteller would almost never do this in
% practice - it gives the player zero new information.

% Washerwoman: discourage putting either token on the washerwoman player
:~ reminder_on(ww_townsfolk, WW, _), assigned(0, WW, washerwoman).
   [10@1, WW, ww_townsfolk]

:~ reminder_on(ww_wrong, WW, _), assigned(0, WW, washerwoman).
   [10@1, WW, ww_wrong]

% Librarian: discourage putting either token on the librarian player
:~ reminder_on(lib_outsider, LIB, _), assigned(0, LIB, librarian).
   [10@1, LIB, lib_outsider]

:~ reminder_on(lib_wrong, LIB, _), assigned(0, LIB, librarian).
   [10@1, LIB, lib_wrong]

% Investigator: discourage putting either token on the investigator player
:~ reminder_on(inv_minion, INV, _), assigned(0, INV, investigator).
   [10@1, INV, inv_minion]

:~ reminder_on(inv_wrong, INV, _), assigned(0, INV, investigator).
   [10@1, INV, inv_wrong]

% ---------------------------------------------------------------------------
% Priority 1: Poisoner targeting evil players
% ---------------------------------------------------------------------------
% A Poisoner poisoning their own team on Night 1 (or any night) is legal,
% but almost never strategically sensible. The whole point of poisoning
% is to make good players receive false information.

% Discourage poisoner targeting any evil player (minion or demon)
:~ player_chooses(poisoner, _, point(Target), _),
   actual_category(Target, evil).
   [10@1, Target]

% ---------------------------------------------------------------------------
% Priority 1: Fortune Teller red herring on self
% ---------------------------------------------------------------------------
% The Fortune Teller's red herring is usually placed on a player other than
% themselves. Putting it on the FT would mean they always ping themselves
% as demon, which is odd (though legal).

:~ reminder_on(ft_red_herring, FT, _), assigned(0, FT, fortune_teller).
   [5@1, FT]

% ---------------------------------------------------------------------------
% Future considerations (commented out for now):
% ---------------------------------------------------------------------------

% Spy registering as good to ALL abilities on the same night
% (A Spy would usually want to cause at least some confusion)
% :~ [complex condition for "spy registered as good everywhere"]

% Recluse registering as evil to ALL abilities on the same night
% (Similar - Recluse usually wants some plausible deniability)
% :~ [complex condition for "recluse registered as evil everywhere"]


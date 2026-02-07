% Reminder Token Metadata

% Ownership: which role each reminder token belongs to (used for grimoire display grouping)
reminder_belongs_to(drunk_is_drunk, drunk).
reminder_belongs_to(poi_poisoned, poisoner).
reminder_belongs_to(ww_townsfolk, washerwoman).
reminder_belongs_to(ww_wrong, washerwoman).
reminder_belongs_to(lib_outsider, librarian).
reminder_belongs_to(lib_wrong, librarian).
reminder_belongs_to(inv_minion, investigator).
reminder_belongs_to(inv_wrong, investigator).
reminder_belongs_to(ft_red_herring, fortune_teller).
reminder_belongs_to(but_master, butler).
reminder_belongs_to(sla_no_ability, slayer).
reminder_belongs_to(monk_protected, monk).
reminder_belongs_to(imp_dead, imp).

% Display labels: human-readable names for the web UI grimoire visualization
reminder_label(drunk_is_drunk, "Is the Drunk").
reminder_label(poi_poisoned, "Poisoned").
reminder_label(ww_townsfolk, "Townsfolk").
reminder_label(ww_wrong, "Wrong").
reminder_label(lib_outsider, "Outsider").
reminder_label(lib_wrong, "Wrong").
reminder_label(inv_minion, "Minion").
reminder_label(inv_wrong, "Wrong").
reminder_label(ft_red_herring, "Red herring").
reminder_label(but_master, "Master").
reminder_label(sla_no_ability, "No ability").
reminder_label(monk_protected, "Protected").
reminder_label(imp_dead, "Dead").

% Self-exclusion: info roles can't place their own tokens on themselves (you can't learn about yourself)
never_applied_to(ww_townsfolk, washerwoman).
never_applied_to(ww_wrong, washerwoman).
never_applied_to(lib_outsider, librarian).
never_applied_to(lib_wrong, librarian).
never_applied_to(inv_minion, investigator).
never_applied_to(inv_wrong, investigator).
never_applied_to(but_master, butler).

% Enforce the self-exclusion: forbid a token from being placed on the player who owns that role
:- never_applied_to(Token, Role), reminder_on(Token, P, _), assigned(0, P, Role).

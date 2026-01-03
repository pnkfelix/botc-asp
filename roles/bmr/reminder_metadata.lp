% BMR Reminder Token Metadata

% Grandmother
reminder_belongs_to(gm_grandchild, grandmother).
reminder_label(gm_grandchild, "Grandchild").

% Sailor
reminder_belongs_to(sailor_drunk, sailor).
reminder_label(sailor_drunk, "Drunk").

% Chambermaid - no reminder tokens, just info

% Exorcist
reminder_belongs_to(exorcist_chosen, exorcist).
reminder_label(exorcist_chosen, "Chosen").

% Innkeeper
reminder_belongs_to(ik_protected, innkeeper).
reminder_belongs_to(ik_drunk, innkeeper).
reminder_label(ik_protected, "Protected").
reminder_label(ik_drunk, "Drunk").

% Gambler
reminder_belongs_to(gambler_dead, gambler).
reminder_label(gambler_dead, "Dead").

% Gossip
reminder_belongs_to(gossip_dead, gossip).
reminder_label(gossip_dead, "Dead").

% Courtier
reminder_belongs_to(courtier_drunk, courtier).
reminder_label(courtier_drunk, "Drunk 3").

% Professor
reminder_belongs_to(professor_alive, professor).
reminder_label(professor_alive, "Alive").

% Minstrel
reminder_belongs_to(minstrel_drunk, minstrel).
reminder_label(minstrel_drunk, "Drunk").

% Tea Lady - no tokens, passive ability

% Pacifist - no tokens, passive ability

% Fool
reminder_belongs_to(fool_used, fool).
reminder_label(fool_used, "Used").

% Tinker
reminder_belongs_to(tinker_dead, tinker).
reminder_label(tinker_dead, "Dead").

% Moonchild
reminder_belongs_to(moonchild_dead, moonchild).
reminder_label(moonchild_dead, "Dead").

% Goon
reminder_belongs_to(goon_drunk, goon).
reminder_label(goon_drunk, "Drunk").

% Lunatic - no tokens, just info

% Godfather
reminder_belongs_to(gf_dead, godfather).
reminder_label(gf_dead, "Dead").

% Devil's Advocate
reminder_belongs_to(da_survives, devils_advocate).
reminder_label(da_survives, "Survives").

% Assassin
reminder_belongs_to(assassin_dead, assassin).
reminder_label(assassin_dead, "Dead").

% Mastermind - no tokens, passive ability

% Zombuul
reminder_belongs_to(zombuul_dead, zombuul).
reminder_belongs_to(zombuul_appears_dead, zombuul).
reminder_label(zombuul_dead, "Dead").
reminder_label(zombuul_appears_dead, "Appears Dead").

% Pukka
reminder_belongs_to(pukka_poisoned, pukka).
reminder_belongs_to(pukka_dead, pukka).
reminder_label(pukka_poisoned, "Poisoned").
reminder_label(pukka_dead, "Dead").

% Shabaloth
reminder_belongs_to(shabaloth_dead, shabaloth).
reminder_belongs_to(shabaloth_alive, shabaloth).
reminder_label(shabaloth_dead, "Dead").
reminder_label(shabaloth_alive, "Alive").

% Po
reminder_belongs_to(po_dead, po).
reminder_label(po_dead, "Dead").

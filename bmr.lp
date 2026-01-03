% Bad Moon Rising Script

#include "roles/bmr/base.lp".
#include "roles/bmr/registration.lp".
#include "roles/bmr/reminder_metadata.lp".

% Townsfolk
#include "roles/bmr/townsfolk/grandmother.lp".
#include "roles/bmr/townsfolk/sailor.lp".
#include "roles/bmr/townsfolk/chambermaid.lp".
#include "roles/bmr/townsfolk/exorcist.lp".
#include "roles/bmr/townsfolk/innkeeper.lp".
#include "roles/bmr/townsfolk/gambler.lp".
#include "roles/bmr/townsfolk/gossip.lp".
#include "roles/bmr/townsfolk/courtier.lp".
#include "roles/bmr/townsfolk/professor.lp".
#include "roles/bmr/townsfolk/minstrel.lp".
#include "roles/bmr/townsfolk/tea_lady.lp".
#include "roles/bmr/townsfolk/pacifist.lp".
#include "roles/bmr/townsfolk/fool.lp".

% Outsiders
#include "roles/bmr/outsiders/tinker.lp".
#include "roles/bmr/outsiders/moonchild.lp".
#include "roles/bmr/outsiders/goon.lp".
#include "roles/bmr/outsiders/lunatic.lp".

% Minions
#include "roles/bmr/minions/godfather.lp".
#include "roles/bmr/minions/devils_advocate.lp".
#include "roles/bmr/minions/assassin.lp".
#include "roles/bmr/minions/mastermind.lp".

% Demons
#include "roles/bmr/demons/zombuul.lp".
#include "roles/bmr/demons/pukka.lp".
#include "roles/bmr/demons/shabaloth.lp".
#include "roles/bmr/demons/po.lp".

% Game end conditions (depends on demon/minion logic above)
#include "game_end.lp".

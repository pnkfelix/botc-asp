% Trouble Brewing Script

#include "roles/tb/base.lp".
#include "roles/tb/registration.lp".
#include "roles/tb/reminder_metadata.lp".

% Townsfolk
#include "roles/tb/townsfolk/washerwoman.lp".
#include "roles/tb/townsfolk/librarian.lp".
#include "roles/tb/townsfolk/investigator.lp".
#include "roles/tb/townsfolk/chef.lp".
#include "roles/tb/townsfolk/empath.lp".
#include "roles/tb/townsfolk/fortune_teller.lp".
#include "roles/tb/townsfolk/undertaker.lp".
#include "roles/tb/townsfolk/monk.lp".
#include "roles/tb/townsfolk/ravenkeeper.lp".
#include "roles/tb/townsfolk/virgin.lp".
#include "roles/tb/townsfolk/slayer.lp".
#include "roles/tb/townsfolk/soldier.lp".
#include "roles/tb/townsfolk/mayor.lp".

% Outsiders
#include "roles/tb/outsiders/butler.lp".
#include "roles/tb/outsiders/drunk.lp".
#include "roles/tb/outsiders/recluse.lp".
#include "roles/tb/outsiders/saint.lp".

% Minions
#include "roles/tb/minions/poisoner.lp".
#include "roles/tb/minions/spy.lp".
#include "roles/tb/minions/scarlet_woman.lp".
#include "roles/tb/minions/baron.lp".

% Demon
#include "roles/tb/demons/imp.lp".

% Game end conditions (depends on demon/minion logic above)
#include "game_end.lp".

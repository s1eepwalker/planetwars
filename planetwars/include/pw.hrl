%rr("planetwars/include/pw.hrl").
-define(GRAY, "\e[1;30m").
-define(RED, "\e[1;31m").
-define(GREEN, "\e[1;32m").
-define(YELLOW, "\e[1;33m").
-define(BLUE, "\e[1;34m").
-define(MAGENTA, "\e[1;35m").
-define(CYAN, "\e[1;36m").
-define(NORM, "\e[0m").

-type player_id() :: 1..20.
-type planet_id() :: 1..100.
-type fleet()     :: pos_integer().
-type turn()      :: 0..200.

-record(planet,
	{
		id = 1                 :: planet_id(),
		x = 0                  :: integer(),
		y = 0                  :: integer(),
		increment = 1          :: integer(),
		owner_id = 0           :: player_id(),
		fleet = 1              :: fleet(),
		confederate = unknown  :: unknown | ally | enemy | team1 | team2 | neutral
	}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(message, {
		type = no_msg          :: im_here | attack | no_msg,
		player_id = 0          :: player_id() | 0,
		turn = 0               :: turn() | 0,
		id_from = 0            :: planet_id() | 0,
		id_to = 0              :: planet_id() | 0,
		fleet = 0              :: fleet() | 0
	}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(order,
	{	                       % wait or {PId1, PId2, Fleet}
		fleet_command = wait   :: wait | {planet_id(), planet_id(), fleet()},
		message = #message{}   :: #message{}
	}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(player, {
		id = 0                     :: player_id(),
		turn = 0                   :: turn(),
		allies = []                :: [player_id()],
		searching_ally = true      :: boolean(),
		last_message = #message{}  :: #message{},
		attack_list = []           :: [{planet_id(), turn()}]
	}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#!/usr/bin/env escript
%%! -config config/single.config -setcookie 123 -sname pw -pa planetwars/ebin/ _checkouts/lager/ebin _checkouts/goldrush/ebin _checkouts/sync/ebin -s lager
-mode(compile).

-include("planetwars/include/pw.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_line() ->
	Line = io:get_line("> "),
	case Line of
		eof -> eof;
		_ ->
			LineWithoutNL = string:strip(string:strip(Line, both, 13), both, 10),
			string:tokens(LineWithoutNL, " ")
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_world(eof) ->
	ok;
read_world(["Y"|_] = L) when length(L) > 1 ->
	gen_server:cast(starbase, {your_id, list_to_integer(lists:nth(2, L))}),
	read_world(read_line());
read_world(["M"|_] = L) when length(L) > 1 ->
	gen_server:cast(starbase,
		{message, util:decode_message(list_to_integer(lists:nth(2, L)))}),
	read_world(read_line());
read_world(["P"|_] = L) when length(L) > 6 ->
	P = #planet{
		id = list_to_integer(lists:nth(2, L)),
		x = list_to_integer(lists:nth(3, L)),
		y = list_to_integer(lists:nth(4, L)),
		increment = list_to_integer(lists:nth(5, L)),
		owner_id = list_to_integer(lists:nth(6, L)),
		fleet = list_to_integer(lists:nth(7, L))
	},
	gen_server:cast(starbase, {planetinfo, P}),
	read_world(read_line());
read_world(["."]) ->
	gen_server:cast(starbase, {wait_decision, self(), now()}),
	ok;
read_world(M) ->
	io:format("> " ?RED "Unknown message ~p~n" ?NORM, [M]),
	read_world(read_line()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
main(_Args) ->
	net_kernel:start([pw, shortnames]),
	% io:format("node = ~p, cookie ~p~n", [node(), erlang:get_cookie()]),
	application:start(planetwars),
	working_loop().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
working_loop() ->
	read_world(read_line()),

	% wait decision
	receive
		{'$gen_cast', {timeout, _}} ->
			io:format(?RED ".~n" ?NORM);
		{'$gen_cast', {order, _PlayerId, #order{fleet_command = Cmd, message = Msg}}} ->
			case Cmd of
				wait -> ok;
				{Pl1, Pl2, Fleet} ->
					io:format("F ~p ~p ~p~n", [Pl1, Pl2, Fleet])
			end,
			case Msg #message.type =/= no_msg of
				true ->
					io:format("M ~p~n", [util:encode_message(Msg)]);
				_ -> ok
			end,
			io:format(".~n")
	after
		1000 ->
			%TIMED OUT!
			io:format(".~n")
	end,
	working_loop().



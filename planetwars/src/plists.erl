-module(plists).

-export([pmap/2, pfirst/2, pfirst_success/4, pforeach/2,npforeach/2, test/0]).


pmap(F, L) ->
  S = self(),
  Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
  pmap_gather(Pids).

pmap_gather([H|T]) ->
  receive
    {H, Ret} ->
      % io:format("RECEIVED ~p~n", [Ret]),
      [Ret|pmap_gather(T)]
  end;
pmap_gather([]) ->
  [].

pmap_f(Parent, F, I) ->
  Parent ! {self(), (catch F(I))}.



pfirst(F, L) ->
  S = self(),
  Pid = spawn(fun() -> pfirst_0(S,F,L) end),
  receive {Pid, Ret} -> Ret end.

pfirst_0(Parent, F, L) ->
  S = self(),
  _Pids = lists:map(fun(I) -> spawn(fun() -> pmap_f(S, F, I) end) end, L),
  receive
    {_Pid, Ret} -> Parent ! {S, Ret}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Parallel call function F(I) || I <- L and get fisrt success result
% Pred = fun(X) -> true or false end
% Timeout - how to wait

pfirst_success(F, L, Pred, Timeout) ->
  S = self(),
  Pid = spawn_link(fun() -> pfirst_0_success(S,F,L, Pred) end),
  receive {Pid, Ret, Pids} ->
      [exit(P, kill) || P <- Pids],
      Ret
    after Timeout ->
      exit(Pid, kill),
      timeout
  end.


pfirst_0_success(Parent, F, L, Pred) ->
  S = self(),
  Pids = lists:map(fun(I) -> spawn_link(fun() -> pmap_f(S, F, I) end) end, L),
  pfirst_success_wait(S, length(Pids), Parent, Pred, Pids).


pfirst_success_wait(S, 0, Parent, _Pred, _Pids) -> Parent ! {S, no_success, []};
pfirst_success_wait(S, Len, Parent, Pred, Pids) ->
  receive
    {_Pid, Ret} ->
      case Pred(Ret) of
        true ->
          Parent ! {S, Ret, Pids};
        _ ->
          pfirst_success_wait(S, Len - 1, Parent, Pred, Pids)
      end
  end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



pforeach(F, L) ->
  S = self(),
  Pids = pmap(fun(I) -> spawn(fun() -> pforeach_f(S,F,I) end) end, L),
  pforeach_wait(Pids).

pforeach_wait([H|T]) ->
  receive
    H -> pforeach_wait(T)
  end;
pforeach_wait([]) -> ok.

pforeach_f(Parent, F, I) ->
  _ = (catch F(I)),
  Parent ! self().




npforeach(F, L) ->
  S = self(),
  Pid = spawn(fun() -> npforeach_0(S,F,L) end),
  receive Pid -> ok end.

npforeach_0(Parent,F,L) ->
  S = self(),
  Pids = pmap(fun(I) -> spawn(fun() -> npforeach_f(S,F,I) end) end, L),
  npforeach_wait(S,length(Pids)),
  Parent ! S.

npforeach_wait(_S,0) -> ok;
npforeach_wait(S,N) ->
  receive
    S -> npforeach_wait(S,N-1)
  end.

npforeach_f(Parent, F, I) ->
  _ = (catch F(I)),
  Parent ! Parent.



test() ->
  F1 = fun() ->
    io:format("F1 starting ...~n", []),
    timer:sleep(2500),
    io:format("F1 Done!~n", []),
    3
  end,

  F2 = fun() ->
    io:format("F2 starting ...~n", []),
    timer:sleep(1000),
    io:format("F2 Done!~n", []),
    3
  end,

  F3 = fun() ->
    io:format("F3 starting ...~n", []),
    timer:sleep(4000),
    io:format("F3 Done!~n", []),
    3
  end,

  Pred = fun(X) -> X == 3 end,


  io:format("START~n", []),
  Ret = pfirst_success(fun(F) -> F() end, [F1, F2, F3], Pred, 1100),
  io:format("END RET= ~p~n", [Ret]).


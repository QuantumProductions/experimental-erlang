-module(cell).
-compile(export_all).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

position_test() ->
  {ok, Cell} = cell:go("Ship"),
  s:s(Cell, {delta_v, [2, 5]}),
  s:s(Cell, {tick, 0}),
  [[{2, 2}, {5, 5}]] = s:s(Cell, info),
  s:s(Cell, {delta_v, [1, 3]}),
  s:s(Cell, {tick, 0}),
  [[{3, 1}, {8, 3}]] = s:s(Cell, info).

default_position_test() ->
  {ok, Cell} = cell:go("Ship", #{position => [{20,0}, {50,0}]}),
  [[{0, 0}, {0, 0}]] = s:s(Cell, info).

handle_call(info, _, State) ->
  Info = lists:map(fun(P) -> s:s(P, info) end, State),
  {reply, Info, State};
handle_call({Message, Contents}, _, State) ->
  Result = lists:map(fun(P) -> s:s(P, {Message, Contents}) end, State),
  {reply, Result, State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

go (TypeName) ->
  go(TypeName, #{}).
go(TypeName, Defaults) ->
  Modules = compositions:compositions(TypeName),
  gen_server:start_link(?MODULE, {Modules, Defaults}, []).
  
handle_cast(_, State) ->
  {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State }.

terminate(normal, State) ->
    io:format("Status.~p~n", [State]),
    ok.

init({Modules, Defaults}) -> 
  Started = lists:map(fun(M) -> 
    case maps:is_key(M, Defaults) of
      true ->  M:go(maps:get(M, Defaults));
      false -> M:go()
    end
  end, Modules),
  Pids = lists:foldl(fun({ok, Pid}, StartingPids) -> lists:append(StartingPids, [Pid]) end, [], Started),
  {ok, Pids}.

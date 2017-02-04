-module(position).
-compile(export_all).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

update_test() ->
  {ok, P} = position:go(),
  [{0, 0}, {0, 3}] = s:s(P, {delta_v, [0, 2]}),
  [{0, 0}, {2, 2}] = s:s(P, {tick, 0}).

assignDeltaVs([], _Dimensions, NewDimensions) ->
  NewDimensions;
assignDeltaVs([HDV | T], [{P, _}  | Dimensions], NewDimensions) ->
  assignDeltaVs(T, Dimensions, lists:append(NewDimensions, [{P, HDV}])).
assignDeltaVs(VDs, Dimensions) ->
  assignDeltaVs(VDs, Dimensions, []).

handle_call(info, _, State) ->
  {reply, State, State};
handle_call({tick, _Delta}, _, Dimensions) ->
  NewState = lists:map(fun({D, VD}) -> {D + VD, VD} end, Dimensions),
  {reply, NewState, NewState};
handle_call({delta_v, VDs}, _, Dimensions) ->
  NewState = assignDeltaVs(VDs, Dimensions),
  {reply, NewState, NewState};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State};
handle_call(_, _, State) ->
  {reply, unexpected, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}. 

go() ->
  go([]).
go(Arguments) ->
  gen_server:start_link(?MODULE, Arguments, []).

handle_cast(_, State) ->
  {noreply, State}.
handle_info(Msg, State) ->
  io:format("Unexpected message: ~p~n",[Msg]),
  {noreply, State}.

terminate(normal, State) ->
    io:format("Status.~p~n", [State]),
    ok.

init([]) -> 
  {ok, [{0, 0}, {0, 0}]};
init(Defaults) ->
  {ok, Defaults}.

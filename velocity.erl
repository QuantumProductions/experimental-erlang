-module(velocity).
-compile(export_all).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

update_test() ->
  {ok, V} = velocity:go([0,0,0]),
  [1,0,0] = s:s(V, {update, [{1,1}]}),
  [3,0,5] = s:s(V, {update, [{1,3}, {3, 5}]}).

% Accepts [1,2,3] or
% Accepts [{1,3}, {2, -11}]
updateVelocity(Vectors, S) when length(Vectors) =:= length(S) ->
  {ok, Vectors};
updateVelocity(Vectors, S) when length(Vectors) >= length(S) ->
  {error, bad_vector};
updateVelocity([], Vectors) ->
  {ok, Vectors};
updateVelocity([{Index, Value} | T], Vectors) ->
  updateVelocity(T, s:replacenth(Index, Value, Vectors)).

handle_call(info, _, State) ->
  {reply, State, State};
handle_call({update, Vectors}, _, S) ->
  case updateVelocity(Vectors, S) of
    {ok, NewState} -> {reply, NewState, NewState};
    {error, Error} -> {reply, {error, Error}, S}
  end;
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

% init with an integer defaults to [0] *N
init([]) -> 
  {ok, [0, 0]};
init(Defaults) ->
  {ok, Defaults}.
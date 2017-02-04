-module(compositions).
-export([compositions/1]).

-define(COMPOSITIONS, #{"Ship" => [position]}).

compositions(Name) ->
  maps:get(Name, ?COMPOSITIONS).
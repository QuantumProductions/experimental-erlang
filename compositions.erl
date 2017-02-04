-module(compositions).
-export([compositions/1]).

-define(COMPOSITIONS, #{"Ship" => [position]}).
% -define(COMPOSITIONS, #{"Ship" => [matter]}).
where matter has position, velocity

compositions(Name) ->
  maps:get(Name, ?COMPOSITIONS).
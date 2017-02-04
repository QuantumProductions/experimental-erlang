-module(s).
-export([s/2]).
-export([replacenth/3]).
% Shortcuts.

% Send
s(Pid, Message) ->
  gen_server:call(Pid, Message).

replacenth(Index,Value,List) ->
 replacenth(Index-1,Value,List,[],0).

replacenth(ReplaceIndex,Value,[_|List],Acc,ReplaceIndex) ->
 lists:reverse(Acc)++[Value|List];
replacenth(ReplaceIndex,Value,[V|List],Acc,Index) ->
 replacenth(ReplaceIndex,Value,List,[V|Acc],Index+1).
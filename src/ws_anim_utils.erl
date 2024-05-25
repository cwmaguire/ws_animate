-module(ws_anim_utils).

-export([json/1]).
-export([info/1]).
-export([log/1]).


json(Map) ->
  iolist_to_binary(json:encode(Map)).

info(Map) ->
    json(Map#{type => <<"info">>}).

log(Bin) ->
    json(#{type => <<"log">>, log => Bin}).


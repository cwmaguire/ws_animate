-module(ws_anim_utils).

-export([json/1]).
-export([info/1]).
-export([log/1]).
-export([tuple_to_color/1]).
-export([color_to_tuple/1]).


json(Map) ->
  iolist_to_binary(json:encode(Map)).

info(Map) ->
    json(Map#{type => <<"info">>}).

log(Bin) ->
    json(#{type => <<"log">>, log => Bin}).

-define(ZERO_PAD_HEX, "~2.16.0B").

tuple_to_color({R, G, B}) ->
    String =
        io_lib:format("#"
                      ?ZERO_PAD_HEX
                      ?ZERO_PAD_HEX
                      ?ZERO_PAD_HEX,
                      [R, G, B]),
    list_to_binary(String).

color_to_tuple(<<"#",
                 R:2/binary,
                 G:2/binary,
                 B:2/binary>>) ->
    Red = b2hex(R),
    Green = b2hex(G),
    Blue = b2hex(B),
    {Red, Green, Blue}.

b2hex(Bin) ->
    binary_to_integer(Bin, 16).

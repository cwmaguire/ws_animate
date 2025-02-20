-module(ws_anim_utils).

-export([json/1]).
-export([info/1]).
-export([log/1]).
-export([tuple_to_color/1]).
-export([color_to_tuple/1]).
-export([send_input_control/5]).
-export([input/4]).

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

send_input_control(Channel, Name, Type, Field, Value) ->
    InputControl = input(Name, Type, Field, Value),
    Channel ! {send, {Name, control}, InputControl}.

input(AnimatorName, Type, Field, Value) ->
    Id = <<AnimatorName/binary, "_", Field/binary, "_", Type/binary>>,
    Input = #{type => <<"control">>,
              cmd => Type,
              id => Id,
              name => AnimatorName,
              field => Field,
              value => Value,
              label => <<AnimatorName/binary, " ", Field/binary>>},
    ws_anim_utils:json(Input).

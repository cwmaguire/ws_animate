-module(ws_anim_animate_convolution).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                device_id = <<"cde819aad3a5f7da8759721271d1d3deaf3dbdce8666ecfb5f4180f93f5e0d00">>,
                x,
                y,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    %% TODO wait for a device ID to be set from the web page
    DeviceId = (#state{})#state.device_id,
    InfoMsg = #{info => <<"send_image">>,
                deviceId => DeviceId,
                animatorName => Name},
    Channel ! {send, info, ?utils:info(InfoMsg)},
    #state{name = Name,
           channel = Channel}.

animate(#{data := Image},
        State = #state{name = Name,
                       channel = Channel,
                       device_id = DeviceId})
  when Image /= undefined ->
    io:format("ws_anim_animate_convolution: image received~n"),
    WebCacheId = list_to_binary(pid_to_list(self())),
    Json = convolute_image(State, Name, WebCacheId, Image),
    Id = {_ZIndex = 100, self(), 1},
    ws_anim_channel:buffer_delete(Channel),
    Channel ! {buffer, {Id, #{json => Json, id => WebCacheId, cached => true}}},
    timer:sleep(5000),
    request_image(DeviceId, Name, Channel),
    maybe_send_name(State, _X = 0, _Y = 0),
    State;
animate(_Settings, State) ->
    %io:format("No image received~n"),
    State.

request_image(DeviceId, Name, Channel) ->
    InfoMsg = #{info => <<"send_image">>,
                deviceId => DeviceId,
                animatorName => Name},
    io:format("requesting image: ~p~n", [InfoMsg]),
    Channel ! {send, info, ?utils:info(InfoMsg)}.

convolute_image(_State, Name, WebCacheId, Image) ->
    Bytes =
        lists:map(fun erlang:binary_to_integer/1,
                  binary:split(Image, <<",">>, [global])),

    _Bytes2 = [trunc(X / 2) || X <- Bytes],

    %% TODO have the web page draw this data directly
    %% onto the canvas, as opposed to creating an image
    %% DOM element and loading it from a URL object.
    %% (partly because I don't know how to load arbitrary
    %%  RGBA image data [bitmap?] into an image elemnt,
    %%  but also because I don't need to: the canvase
    %%  can just draw it.)
    Map = #{type => <<"draw">>,
            cmd => <<"bitmap">>,
            shouldCache => true,
            id => WebCacheId,
            data => Bytes,
            x => 0,
            y => 0,
            w => 160,
            name => Name},
    ws_anim_utils:json(Map).

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       is_showing_name = true},
                X, Y) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => X - ?NAME_OFFSET_X,
          y => Y - ?NAME_OFFSET_Y,
          text => Name,
          font => <<"serif">>,
          font_size => ?NAME_FONT_SIZE,
          font_color => ?NAME_FONT_COLOR,
          name => Name},

    TextJson = ws_anim_utils:json(TextMap),

    Id = {100, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_, _, _) ->
    ok.

send_controls(State = #state{name = Name, channel = Channel}) ->
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"x">>, State#state.x),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"x">>, State#state.y),
    State.

-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(INT_SETTINGS, #{<<"x">> => ?SETTING(x),
                        <<"y">> => ?SETTING(y)}).

set(Setting, Value, State)
  when Setting == <<"x">>;
       Setting == <<"y">> ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          #{Setting := Fun} = ?INT_SETTINGS,
          Fun(State, I);
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for ", Setting/binary>>)},
          State
  end;
set(<<"is_showing_name">>, Value, State) ->
    case Value of
        <<"true">> ->
            State#state{is_showing_name = true};
        <<"false">> ->
            State#state{is_showing_name = false};
        _ ->
            State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid boolean ", Value/binary, " for is_showing_name">>)},
            State
    end;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.

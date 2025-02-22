-module(ws_anim_animate_video).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined :: pid(),
                device_id = <<"cde819aad3a5f7da8759721271d1d3deaf3dbdce8666ecfb5f4180f93f5e0d00">>,
                style = <<"black">>,
                %x = 0,
                %y = 0,
                max_w = 800,
                max_h = 750,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel,
                       device_id = DeviceId,
                       %x = _X,
                       %y = _Y,
                       max_w = W,
                       max_h = H }) ->

    VideoDimensions = video_dimensions(Frame, Name, W, H, DeviceId),

    send(Channel, VideoDimensions),
    maybe_send_name(State),
    State.

video_dimensions(Frame, Name, MaxW, MaxH, DeviceId) ->
    X = Frame rem MaxW,
    Y = Frame rem MaxH,
    DrawInstruction = #{type => <<"draw">>,
                        cmd => <<"video_frame">>,
                        name => Name,
                        device_id => DeviceId,
                        x => X,
                        y => Y,
                        w => 100,
                        h => 100},
    {id(1), ?utils:json(DrawInstruction)}.

id(X) ->
    ZIndex = 100,
    {ZIndex, self(), X}.

send(Channel, BufferObject) ->
    Channel ! {buffer, BufferObject}.

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       is_showing_name = true}) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => 290,
          y => 290,
          text => Name,
          font => <<"serif">>,
          font_size => ?NAME_FONT_SIZE,
          font_color => ?NAME_FONT_COLOR,
          name => Name},

    TextJson = ws_anim_utils:json(TextMap),

    Id = {1, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_) ->
    ok.

send_controls(State = #state{name = Name, channel = Channel}) ->
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"style">>, State#state.style),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"max_w">>, State#state.max_w, #{min => 100, max => 650}),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"max_h">>, State#state.max_h, #{min => 100, max => 700}),
    State.

-define(INT_SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(INT_SETTINGS, #{<<"max_w">> => ?INT_SETTING(max_w),
                        <<"max_h">> => ?INT_SETTING(max_h)}).

set(Setting, Value, State)
  when Setting == <<"max_w">>;
       Setting == <<"max_h">> ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          #{Setting := Fun} = ?INT_SETTINGS,
          Fun(State, I);
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for ", Setting/binary>>)},
          State
  end;
set(<<"style">>, Value, State) ->
    State#state{style = Value};
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


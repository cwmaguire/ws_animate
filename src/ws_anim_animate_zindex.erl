-module(ws_anim_animate_zindex).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                width = 200,
                height = 200,
                style = <<"black">>,
                rise = -80,
                min_h = 20,
                min_w = 20,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel}) ->
    {Square, X, Y, ZIndex} = square(State, Frame, Name),
    Id = {ZIndex, self(), 1},
    ws_anim_channel:buffer_delete(Channel),
    Channel ! {buffer, {Id, Square}},
    maybe_send_name(State, X, Y),
    State.

square(State, Frame, Name) ->
    SH = (State#state.height - State#state.min_h) / 2,
    SW = (State#state.width - State#state.min_w) / 2,
    Sine = math:sin((Frame / 10)),
    Cosine = math:cos((Frame / 10)),

    H = (Sine * SH) + SH + State#state.min_h,
    W = (Sine * SW) + SW + State#state.min_w,

    Offset = SW / 2,
    CanvasHalfWidth = (800 - 34) / 2,
    OffsetWidth = CanvasHalfWidth - Offset,

    X = (Cosine * OffsetWidth) + OffsetWidth,
    Y = 250 + (Sine * State#state.rise),
    Z = trunc(Sine * 50 + 50),

    %io:format("Sine: ~p, W: ~p, X: ~p, X+W-800: ~p, Z = ~p~n", [Sine, W, X, X + W - 800, Z]),

    Map = #{type => <<"draw">>,
            cmd => <<"square_filled">>,
            x => X,
            y => Y,
            w => W,
            h => H,
            style => State#state.style,
            name => Name},
    {ws_anim_utils:json(Map), X, Y, Z}.

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
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"width">>, State#state.width),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"height">>, State#state.height),
    ?utils:send_input_control(Channel, Name, <<"color">>, <<"style">>, State#state.style),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"rise">>, State#state.rise),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"min_h">>, State#state.min_h),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"min_w">>, State#state.min_w),
    State.

-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(INT_SETTINGS, #{<<"width">> => ?SETTING(width),
                        <<"height">> => ?SETTING(height),
                        <<"rise">> => ?SETTING(rise),
                        <<"min_h">> => ?SETTING(min_h),
                        <<"min_w">> => ?SETTING(min_w)}).

set(Setting, Value, State)
  when Setting == <<"width">>;
       Setting == <<"height">>;
       Setting == <<"rise">>;
       Setting == <<"min_h">>;
       Setting == <<"min_w">> ->
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


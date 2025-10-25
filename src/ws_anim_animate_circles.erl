-module(ws_anim_animate_circles).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                radius = 30,
                style = <<"black">>,
                number = 5,
                xd = 1,
                yd = 1,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{channel = Channel}) ->
    Circles = circles(State, Frame),
    ws_anim_channel:buffer_delete(Channel),
    [Channel ! {buffer, C} || {C, _X, _Y} <- Circles],
    [{_, X, Y} | _] = Circles,
    maybe_send_name(State, X, Y),
    State.

-define(CANVAS_WIDTH, 800).
-define(CANVAS_HEIGHT, 700).

circles(State, Frame) ->
    Number = State#state.number,
    F = Frame rem 100,
    Circle = fun(I) -> circle(I, F, State) end,
    [Circle(I) || I <- lists:seq(1, Number)].

circle(I,
       F,
       #state{number = N, xd = XD, yd = YD, radius = R, style = _Style, name = Name}) ->
    Id = {_ZIndex = rand:uniform(100), self(), I},
    BaseX = I/N * ?CANVAS_WIDTH,
    BaseY = I/N * ?CANVAS_HEIGHT,
    X = trunc(BaseX + (F * XD)) rem ?CANVAS_WIDTH,
    Y = trunc(BaseY + (F * YD)) rem ?CANVAS_HEIGHT,
    Map = #{type => <<"draw">>,
            cmd => <<"circle">>,
            x => X,
            y => Y,
            r => R,
            style => ?utils:random_color(),
            name => Name},
    {{Id, ws_anim_utils:json(Map)}, X, Y}.

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

    Id = {1, self(), 2},
    Channel ! {buffer, {Id, TextJson}};
maybe_send_name(_, _, _) ->
    ok.

send_controls(State = #state{name = Name, channel = Channel}) ->
    ?utils:send_input_control(Channel, Name, <<"checkbox">>, <<"is_showing_name">>, State#state.is_showing_name),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"radius">>, State#state.radius),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"style">>, State#state.style),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"xd">>, State#state.xd, #{min => -20, max => 20}),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"yd">>, State#state.yd, #{min => -20, max => 20}),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"number">>, State#state.number),
    State.

-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(SETTINGS, #{<<"radius">> => ?SETTING(radius),
                    <<"number">> => ?SETTING(number),
                    <<"xd">> => ?SETTING(xd),
                    <<"yd">> => ?SETTING(yd)}).

set(Setting, Value, State)
  when Setting == <<"radius">>;
       Setting == <<"xd">>;
       Setting == <<"yd">>;
       Setting == <<"number">> ->

  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          #{Setting := Fun} = ?SETTINGS,
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


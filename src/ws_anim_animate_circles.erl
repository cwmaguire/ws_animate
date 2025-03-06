-module(ws_anim_animate_circles).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                radius = 300,
                style = <<"black">>,
                xf = 100,
                yf = 100,
                is_showing_name = false}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel}) ->
    Id = {_ZIndex = 100, self(), 1},
    {Circle, X, Y} = circle(State, Frame, Name),
    Channel ! {buffer, {Id, Circle}},
    maybe_send_name(State, X, Y),
    State.

circle(State, Frame, Name) ->
    XF = State#state.xf,
    YF = State#state.yf,
    FX = Frame rem XF,
    FY = Frame rem YF,
    R = State#state.radius,
    %R = trunc(abs(math:cos((Frame / 100))) * State#state.radius),
    X = trunc((FX / XF) * 820) - 10,
    %X = trunc(abs(math:sin((Frame / 100))) * (800 - R)),
    Y = trunc((FY / YF) * 720) - 10,
    %Y = trunc(abs(math:cos((Frame / 100))) * (700 - R)),
    Map = #{type => <<"draw">>,
            cmd => <<"circle">>,
            x => X,
            y => Y,
            r => R,
            style => State#state.style,
            name => Name},
    {ws_anim_utils:json(Map), X, Y}.

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
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"xf">>, State#state.xf),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"yf">>, State#state.yf),
    State.

-define(SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(SETTINGS, #{<<"radius">> => ?SETTING(radius),
                    <<"xf">> => ?SETTING(xf),
                    <<"yf">> => ?SETTING(yf)}).

set(Setting, Value, State)
  when Setting == <<"radius">>;
       Setting == <<"xf">>;
       Setting == <<"yf">> ->

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


-module(ws_anim_animate_circles).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).

-record(state, {name,
                channel = undefined,
                radius = 300,
                style = <<"black">>,
                is_showing_name = false}).

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(Frame,
        State = #state{name = Name,
                       channel = Channel}) ->
    Id = {_ZIndex = 100, self(), 1},
    {Circle, X, Y} = circle(State, Frame, Name),
    Channel ! {buffer, {Id, Circle}},
    maybe_send_name(State, X, Y),
    State.

circle(State, Frame, Name) ->
    R = trunc(abs(math:cos((Frame / 100))) * State#state.radius),
    X = trunc(abs(math:sin((Frame / 100))) * (800 - R)),
    Y = trunc(abs(math:cos((Frame / 100))) * (700 - R)),
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
    State.

set(<<"radius">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{radius = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for radius">>)},
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


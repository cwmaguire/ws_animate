-module(ws_anim_animate_circles2).

-include("ws_anim.hrl").

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-define(PI, math:pi()).

-record(state, {name,
                channel = undefined,
                radius = 70,
                num_circles = 4,
                num_lines = 50,
                style = <<"black">>,
                x = 350,
                y = 300,
                last_x_y,
                is_showing_name = false,
                lines = []}).

rec_info() -> {record_info(size, state),
               skip(record_info(fields, state))}.

skip(Fields) ->
    lists:map(fun(lines) -> {skip, lines};
                 (F) -> F end,
              Fields).

init(Name, Channel) ->
    #state{name = Name, channel = Channel}.

animate(#{frame := Frame},
        State = #state{name = Name,
                       channel = Channel,
                       lines = Lines,
                       num_lines = NumLines,
                       num_circles = NumCircles,
                       radius = Radius,
                       x = X,
                       y = Y,
                       last_x_y = LastXY}) ->

    {Circles2, {X2, Y2}} = circles(Frame, Name, NumCircles, Radius, X, Y, []),

    Lines2 = lines(LastXY, {X2, Y2}, Name, NumLines, Lines, Frame),
    [send(Channel, C) || C <- Circles2],
    [send(Channel, L) || L <- Lines2],
    maybe_send_name(State),
    State#state{last_x_y = {X2, Y2}, lines = Lines2}.

lines({X1, Y1}, {X2, Y2}, Name, NumLines, Lines0, Frame) ->
    LineId = id((Frame rem NumLines) + 6),
    Line = line(Name, LineId, X1, Y1, X2, Y2),
    lists:sublist([Line | Lines0], NumLines);
lines(_, _, _, _, _, _) ->
    [].

id(X) ->
    ZIndex = 100,
    {ZIndex, self(), X}.

send(Channel, BufferObject) ->
    Channel ! {buffer, BufferObject}.

-define(CIRCLE(Name), #{type => <<"draw">>,
                        cmd => <<"circle">>,
                        style => <<"black">>,
                        name => Name}).

circles(_, _, 0, _, X, Y, Circles) ->
    {Circles, {X, Y}};
circles(Frame, Name, NumCircles, PrevRadius, PrevX, PrevY, Circles = []) ->
    Map0 = ?CIRCLE(Name),
    Map = Map0#{x => PrevX,
                y => PrevY,
                r => PrevRadius},
    DrawInstruction = ws_anim_utils:json(Map),
    Circle = {id(NumCircles), DrawInstruction},
    circles(Frame, Name, NumCircles - 1, PrevRadius, PrevX, PrevY, [Circle | Circles]);
circles(Frame, Name, NumCircles, PrevRadius, PrevX, PrevY, Circles) ->
    Radius = (1 - (1 / NumCircles)) * PrevRadius,
    NextAngle = math:fmod(((1/NumCircles) * Frame), 2 * ?PI),
    X = trunc(PrevX + (math:cos(NextAngle) * (PrevRadius + Radius))),
    Y = trunc(PrevY + (math:sin(NextAngle) * (PrevRadius + Radius))),
    Map0 = ?CIRCLE(Name),
    Map = Map0#{x => X,
                y => Y,
                r => Radius},
    DrawInstruction = ws_anim_utils:json(Map),
    Circle = {id(NumCircles), DrawInstruction},
    circles(Frame, Name, NumCircles - 1, Radius, X, Y, [Circle | Circles]).

line(Name, Id, X1, Y1, X2, Y2) ->
    Map = #{type => <<"draw">>,
            cmd => <<"line">>,
            x1 => X1,
            y1 => Y1,
            x2 => X2,
            y2 => Y2,
            name => Name},
    DrawInstruction = ws_anim_utils:json(Map),
    {Id, DrawInstruction}.

maybe_send_name(#state{name = Name,
                       channel = Channel,
                       is_showing_name = true}) ->
    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => 290,
          y => 290,
          text => Name,
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
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"radius">>, State#state.radius),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"#_circles">>, State#state.num_circles),
    ?utils:send_input_control(Channel, Name, <<"number">>, <<"#_lines">>, State#state.num_lines, #{min => 1, max => 10}),
    ?utils:send_input_control(Channel, Name, <<"textbox">>, <<"style">>, State#state.style),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"x">>, State#state.x, #{min => 100, max => 700}),
    ?utils:send_input_control(Channel, Name, <<"range">>, <<"y">>, State#state.y, #{min => 100, max => 650}),
    State.

-define(INT_SETTING(S), fun(State_, I_) -> State_#state{S = I_} end).

-define(INT_SETTINGS, #{<<"radius">> => ?INT_SETTING(radius),
                        <<"#_circles">> => ?INT_SETTING(num_circles),
                        <<"#_lines">> => fun set_lines/2,
                        <<"x">> => ?INT_SETTING(x),
                        <<"y">> => ?INT_SETTING(y)}).

set(Setting, Value, State)
  when Setting == <<"radius">>;
       Setting == <<"#_circles">>;
       Setting == <<"#_lines">>;
       Setting == <<"x">>;
       Setting == <<"y">> ->
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

set_lines(State = #state{channel = Channel}, I) ->
    % TODO erase old lines if new I < old I
    ws_anim_channel:buffer_delete(Channel, {{'_', self(), '_'}, '_'}),
    State#state{num_lines = I}.

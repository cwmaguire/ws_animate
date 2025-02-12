-module(ws_anim_animate_circles2).

-behaviour(gen_server).

-export([start/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 100).
-define(PI, math:pi()).

-record(state, {name,
                channel = undefined,
                frame = 1,
                radius = 300,
                style = <<"black">>,
                last_x_y}).

start(Name) ->
    Caller = self(),
    gen_server:start(?MODULE, _Args = {Name, Caller}, _Opts = []).

init({Name, Channel}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    {ok, #state{name = Name, channel = Channel}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State0 = #state{frame = Frame}) ->
    State1 = animate(State0),
    {noreply, State1#state{frame = Frame + 1}};
handle_info({set, Field, Value}, State) ->
    {noreply, set(Field, Value, State)};
handle_info(send_controls, State = #state{}) ->
    send_controls(State),
    {noreply, State}.

animate(State = #state{name = Name,
                       channel = Channel,
                       frame = Frame,
                       last_x_y = LastXY}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    Circles = [{Name, id(1), ?PI / 4, 41, ?PI / 100},
               {Name, id(2), ?PI / 3, 27, ?PI / 50},
               {Name, id(3), ?PI / 2, 18, ?PI / 25},
               {Name, id(4), ?PI / 2, 12, ?PI / 12.5},
               {Name, id(5), ?PI / 2, 8, ?PI / 6.25}],
    Acc = {[], {300, 300, 50, Frame}},
    {BufferObjects, {X2, Y2, _, _}} = lists:foldl(fun circle/2, Acc, Circles),
    [send(Channel, B) || B <- BufferObjects],
    case LastXY of
        {X1, Y1} ->
            NumLines = 100,
            LineId = id((Frame rem NumLines) + 6),
            Line = line(Name, LineId, X1, Y1, X2, Y2),
            send(Channel, Line);
        _ ->
            ok
    end,
    State#state{last_x_y = {X2, Y2}}.

id(X) ->
    {self(), X}.

send(Channel, BufferObject) ->
    Channel ! {buffer, BufferObject}.

circle({Name, Id, StartAngle, Radius, Rate}, Acc) ->
    {Circles, {PrevX, PrevY, PrevRad, Frame}} = Acc,
    NextAngle = StartAngle + math:fmod((Rate * Frame), 2 * ?PI),
    X = trunc(PrevX + (math:cos(NextAngle) * (PrevRad + Radius))),
    Y = trunc(PrevY + (math:sin(NextAngle) * (PrevRad + Radius))),
    Map = #{type => <<"draw">>,
            cmd => <<"circle">>,
            x => X,
            y => Y,
            r => Radius,
            style => <<"black">>,
            name => Name},
    DrawInstruction = ws_anim_utils:json(Map),
    Circle = {Id, DrawInstruction},
    {[Circle | Circles], {X, Y, Radius, Frame}}.

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

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"radius">>, State#state.radius)},
    Channel ! {send, control, textbox(Name, <<"style">>, State#state.style)},
    ok.

textbox(AnimatorName, Field, Value) ->
    Id = <<AnimatorName/binary, "_", Field/binary, "_textbox">>,
    Textbox = #{type => <<"control">>,
                cmd => <<"textbox">>,
                id => Id,
                name => Id,
                animator => AnimatorName,
                field => Field,
                value => Value,
                label => <<AnimatorName/binary, " ", Field/binary>>},
    ws_anim_utils:json(Textbox).

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
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.



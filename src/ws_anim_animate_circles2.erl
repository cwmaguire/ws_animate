-module(ws_anim_animate_circles2).

-behaviour(gen_server).

-export([start/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 100).

-record(state, {name,
                channel = undefined,
                frame = 1,
                radius = 300,
                style = <<"black">>}).

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

handle_info(animate, State = #state{frame = Frame}) ->
    animate(State),
    {noreply, State#state{frame = Frame + 1}};
handle_info({set, Field, Value}, State) ->
    {noreply, set(Field, Value, State)};
handle_info(send_controls, State = #state{}) ->
    send_controls(State),
    {noreply, State}.

animate(State = #state{name = Name,
                       channel = Channel,
                       frame = Frame}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    Id = {self(), 1},
    Circle = circle(State, Frame, Name),
    Channel ! {buffer, draw, {Id, Circle}}.

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
    ws_anim_utils:json(Map).

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



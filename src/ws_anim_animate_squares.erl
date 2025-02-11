-module(ws_anim_animate_squares).

-behaviour(gen_server).

-export([start/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 1000).

-record(state, {name,
                channel = undefined,
                frame = 1,
                width = 300,
                height = 300,
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
    %io:format(user, "Value = ~p~n", [Value]),
    %io:format(user, "Field = ~p~n", [Field]),
    {noreply, set(Field, Value, State)};
handle_info(send_controls, State = #state{}) ->
    send_controls(State),
    {noreply, State}.

animate(State = #state{name = Name,
                       channel = Channel,
                       frame = Frame}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),

    %[Channel ! {buffer, draw, square(I, J, Frame, Name)} || {I, J} <- [{0, 2}, {100, -1}, {50, -2}]].
    [Channel ! {buffer, draw, square(State, I, J, Frame, Name)} || {I, J} <- [{0, 2}]].

square(State, _I, _J, Frame, Name) ->
    W = trunc(abs(math:sin((Frame / 100))) * State#state.width),
    H = trunc(abs(math:cos((Frame / 100))) * State#state.height),
    X = trunc(abs(math:sin((Frame / 100))) * (800 - W)),
    Y = trunc(abs(math:cos((Frame / 100))) * (700 - H)),
    Map = #{type => <<"draw">>,
            cmd => <<"square">>,
            x => X,
            y => Y,
            w => W,
            h => H,
            style => State#state.style,
            name => Name},
    ws_anim_utils:json(Map).

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
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

set(<<"width">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{width = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for width">>)},
          State
  end;
set(<<"height">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{height = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for height">>)},
          State
  end;
set(<<"style">>, Value, State) ->
    State#state{style = Value};
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.

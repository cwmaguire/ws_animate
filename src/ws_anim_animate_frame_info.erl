-module(ws_anim_animate_frame_info).

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
                type = fps :: fps | duration | count,
                window = 1 :: integer(),
                prev_time :: integer,
                times = [] :: list(),
                x = 100,
                y = 100,
                font_size = <<"12pt">>,
                font_color = <<"blue">>}).

start(Name) ->
    Caller = self(),
    gen_server:start(?MODULE, _Args = {Name, Caller}, _Opts = []).

init({Name, Channel}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    CurrentTime = erlang:monotonic_time(millisecond),
    {ok, #state{name = Name, channel = Channel, prev_time = CurrentTime}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State = #state{frame = Frame}) ->
    State1 = animate(State),
    {noreply, State1#state{frame = Frame + 1}};
handle_info({set, Field, Value}, State) ->
    {noreply, set(Field, Value, State)};
handle_info(send_controls, State = #state{}) ->
    send_controls(State),
    {noreply, State}.

animate(State = #state{channel = Channel, name = Name}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    ZIndex = 100,
    Id = {ZIndex, self(), 1},
    {State1, FrameInfo, AvgTime} = frame_info(State),
    Channel ! {buffer, {Id, FrameInfo}},

    TimingInfo = ws_anim_utils:info(#{animator => Name, avg_frame_time => AvgTime}),
    %io:format(user, "~p / ~p sending TimingInfo ~p to Channel ~p~n", [self(), Name, TimingInfo, Channel]),
    Channel ! {send, info, TimingInfo},
    State1.

% 5 seconds per side
frame_info(State = #state{window = WindowSeconds,
                          prev_time = PrevTime,
                          times = PrevTimes,
                          name = Name}) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    CurrentDiff = CurrentTime - PrevTime,

    MinimumTime = CurrentTime - (WindowSeconds * 1000),

    NewTimes = [{CurrentTime, CurrentDiff} | PrevTimes],
    CurrentTimes = [TD || TD = {T, _} <- NewTimes, T > MinimumTime],

    Diffs = [D || {_, D} <- CurrentTimes],
    DiffTotal = lists:foldl(fun erlang:'+'/2, 0, Diffs),
    AvgTime =  abs(DiffTotal / length(CurrentTimes)),

    Text = float_to_binary(AvgTime, [{decimals, 3}]),

    TextMap =
        #{type => <<"draw">>,
          cmd => <<"text">>,
          x => State#state.x,
          y => State#state.y,
          text => Text,
          font_size => State#state.font_size,
          font_color => State#state.font_color,
          name => Name},
    State1 = State#state{prev_time = CurrentTime,
                         times = CurrentTimes},
    {State1, ws_anim_utils:json(TextMap), Text}.

send_controls(State = #state{name = Name, channel = Channel}) ->
    %Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    %Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
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
          State#state{x = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for width">>)},
          State
  end;
set(<<"height">>, Value, State) ->
  case catch binary_to_integer(Value) of
      I when is_integer(I) ->
          io:format(user, "I = ~p~n", [I]),
          State#state{y = I};
      _ ->
          State#state.channel ! {send, log, ws_anim_utils:log(<<"Invalid integer ", Value/binary, " for height">>)},
          State
  end;
set(<<"style">>, _Value, _State) ->
    %State#state{style = Value};
    ok;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.



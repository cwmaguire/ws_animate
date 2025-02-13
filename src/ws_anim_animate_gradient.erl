-module(ws_anim_animate_gradient).

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
                width = 800,
                height = 700,
                window_secs = 1 :: integer(),
                prev_time :: integer(),
                times = []}).

start(Name) ->
    Caller = self(),
    gen_server:start(?MODULE, _Args = {Name, Caller}, _Opts = []).

init({Name, Channel}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    CurrentTime = erlang:monotonic_time(millisecond),
    {ok, #state{name = Name,
                channel = Channel,
                prev_time = CurrentTime}}.

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

animate(State = #state{name = Name,
                       channel = Channel,
                       frame = Frame}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    %% TODO make this clear in _channel.erl
    %% A zero in the ID will sort lower than any PID and be drawn first
    ZIndex = 0,
    Id = {ZIndex, self(), 1},
    Square = square(State, Frame, Name),
    Channel ! {buffer, {Id, Square}},

    {State1, TimingInfo} = avg_frame_time(State),
    Channel ! {send, info, TimingInfo},
    State1.

avg_frame_time(State = #state{window_secs = WindowSeconds,
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
    AvgTimeBin = float_to_binary(AvgTime, [{decimals, 3}]),

    State1 = State#state{prev_time = CurrentTime,
                         times = CurrentTimes},
    TimingInfo = ws_anim_utils:info(#{animator => Name, avg_frame_time => AvgTimeBin}),
    {State1, TimingInfo}.

% 5 seconds per side
square(#state{width = W, height = H}, Frame, Name) ->
    {X1, Y1} =
        case Frame rem 200 of
            Top when Top =< 50 ->
                X1_ = W / 50 * Top,
                Y1_ = 0,
                {X1_, Y1_};
            Right when Right =< 100 ->
                X1_ = W,
                Y1_ = H / 50 * (Right - 50),
                {X1_, Y1_};
            Bottom when Bottom =< 150 ->
                X1_ = W - (W / 50 * (Bottom - 100)),
                Y1_ = H,
                {X1_, Y1_};
            Left ->
                X1_ = 0,
                Y1_ = H - (H / 50 * (Left - 150)),
                {X1_, Y1_}
        end,
    X2 = W - X1,
    Y2 = H - Y1,

    GradientMap =
        #{%type => <<"draw">>,
          %cmd => <<"gradient">>,
          gx1 => X1,
          gy1 => Y1,
          gx2 => X2,
          gy2 => Y2,
          stop1 => #{stop => 0, color => <<"black">>},
          stop2 => #{stop => 1, color => <<"red">>}},

    SquareMap =
        #{type => <<"draw">>,
          cmd => <<"square_filled">>,
          x => 0,
          y => 0,
          w => W,
          h => H,
          style => GradientMap,
          name => Name},
    ws_anim_utils:json(SquareMap).

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
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
set(<<"style">>, _Value, _State) ->
    %State#state{style = Value};
    ok;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.


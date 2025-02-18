-module(ws_anim_animate_frame_info).

-export([init/2]).
-export([animate/2]).
-export([set/3]).
-export([send_controls/1]).
-export([rec_info/0]).

-record(state, {name,
                channel = undefined,
                x = 100,
                y = 100,
                font_size = <<"12pt">>,
                font_color = <<"blue">>,
                window_secs = 1 :: integer(),
                prev_time :: integer,
                times = [] :: list()}).

rec_info() -> {record_info(size, state),
               record_info(fields, state)}.

init(Name, Channel) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    #state{name = Name,
           channel = Channel,
           prev_time = CurrentTime}.

animate(_Settings,
        State = #state{channel = Channel}) ->
    ZIndex = 100,
    Id = {ZIndex, self(), 1},
    {State1, FrameInfo} = frame_info(State),
    Channel ! {buffer, {Id, FrameInfo}},
    State1.

% 5 seconds per side
frame_info(State = #state{window_secs = WindowSeconds,
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
    {State1, ws_anim_utils:json(TextMap)}.

send_controls(State = #state{name = _Name, channel = _Channel}) ->
    %Channel ! {send, control, textbox(Name, <<"width">>, State#state.width)},
    %Channel ! {send, control, textbox(Name, <<"height">>, State#state.height)},
    State.

%textbox(AnimatorName, Field, Value) ->
%    Id = <<AnimatorName/binary, "_", Field/binary, "_textbox">>,
%    Textbox = #{type => <<"control">>,
%                cmd => <<"textbox">>,
%                id => Id,
%                name => Id,
%                animator => AnimatorName,
%                field => Field,
%                value => Value,
%                label => <<AnimatorName/binary, " ", Field/binary>>},
%    ws_anim_utils:json(Textbox).

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
set(<<"style">>, _Value, State) ->
    %State#state{style = Value};
    State;
set(Field, _Value, State) ->
    State#state.channel ! {send, log, ws_anim_utils:log(<<"Unrecognized field: ", Field/binary>>)},
    State.



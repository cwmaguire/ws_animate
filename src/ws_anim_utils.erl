-module(ws_anim_utils).

-export([json/1]).
-export([info/1]).
-export([log/1]).
-export([avg_frame_time/3]).


json(Map) ->
  iolist_to_binary(json:encode(Map)).

info(Map) ->
    json(Map#{type => <<"info">>}).

log(Bin) ->
    json(#{type => <<"log">>, log => Bin}).

avg_frame_time(WindowSeconds, PrevTime, PrevTimes) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    CurrentDiff = CurrentTime - PrevTime,

    MinimumTime = CurrentTime - (WindowSeconds * 1000),

    NewTimes = [{CurrentTime, CurrentDiff} | PrevTimes],
    CurrentTimes = [TD || TD = {T, _} <- NewTimes, T > MinimumTime],

    Diffs = [D || {_, D} <- CurrentTimes],
    DiffTotal = lists:foldl(fun erlang:'+'/2, 0, Diffs),
    AvgTime =  abs(DiffTotal / length(CurrentTimes)),
    AvgTimeBin = float_to_binary(AvgTime, [{decimals, 3}]),

    {AvgTimeBin, CurrentTime, CurrentTimes}.

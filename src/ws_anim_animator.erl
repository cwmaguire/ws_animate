-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/1]).
-export([start/2]).
-export([get_state/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {name,
                channel = undefined,
                frame = 1,
                frame_millis = 50,
                animator_module,
                animator_state,
                window_secs = 1 :: integer(),
                prev_time :: integer,
                times = [] :: list(),
                running = true :: true | false | freeze}).


start(StateMap = #{}) ->
    Caller = self(),
    gen_server:start(?MODULE,
                     _Args = {StateMap, Caller},
                     _Opts = []).

start(Name, Module) ->
    Caller = self(),
    gen_server:start(?MODULE,
                     _Args = {Name, Caller, Module},
                     _Opts = []).

get_state(Animator) ->
    gen_server:call(Animator, get_state).

init({_LoadedState = #{name := Name,
                       module := AModBin,
                       frame_millis := FrameMillis,
                       frame := Frame,
                       state := LoadedState},
      Channel}) ->
    AMod = binary_to_atom(AModBin),
    {ok, State0 = #state{animator_state = AState0}} =
        init({Name, Channel, AMod}),
    AState = load_state(AMod, AState0, LoadedState),
    {ok, State0#state{animator_state = AState,
                      frame_millis = FrameMillis,
                      frame = Frame,
                      name = Name}};
init({Name, Channel, AnimatorModule}) ->
    State = #state{},
    erlang:send_after(State#state.frame_millis, self(), animate),
    CurrentTime = erlang:monotonic_time(millisecond),
    AState = AnimatorModule:init(Name, Channel),
    {ok, State#state{name = Name,
                     channel = Channel,
                     animator_module = AnimatorModule,
                     animator_state = AState,
                     prev_time = CurrentTime}}.

handle_call(get_state,
            {_From, _},
            State = #state{animator_module = AMod,
                           animator_state = AState,
                           frame_millis = FrameMillis,
                           frame = Frame,
                           name = Name}) ->
    SavedState = #{module => AMod,
                   frame_millis => FrameMillis,
                   frame => Frame,
                   name => Name,
                   state => get_state(AMod, AState)},
    {reply, SavedState, State};
handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State = #state{running = false}) ->
    {noreply, State};
handle_info(animate, State = #state{frame = Frame}) ->
    State1 = animate(State),
    NextFrame =
       case State#state.running of
           freeze ->
               Frame;
           true ->
               Frame + 1
       end,
    {noreply, State1#state{frame = NextFrame}};
handle_info({set, Field, Value},
            State = #state{animator_state = AState,
                           animator_module = AMod}) ->
    AState1 = AMod:set(Field, Value, AState),
    State1 = State#state{animator_state = AState1},
    State2 =
        case Field of
            <<"frame_millis">> ->
               case catch binary_to_integer(Value) of
                   I when is_integer(I) ->
                       State1#state{frame_millis = I};
                   _ ->
                       State1
               end;
            _ ->
                State1
        end,
    {noreply, State2};
handle_info(send_controls,
            State = #state{animator_state = AState,
                           animator_module = AMod}) ->
    AState1 = AMod:send_controls(AState),
    send_controls(State),
    {noreply, State#state{animator_state = AState1}};
handle_info(stop, State) ->
    {noreply, State#state{running = false}};
handle_info(start, State) ->
    erlang:send_after(State#state.frame_millis, self(), animate),
    {noreply, State#state{running = true}};
handle_info(freeze, State) ->
    {noreply, State#state{running = freeze}};
handle_info(unfreeze, State) ->
    erlang:send_after(State#state.frame_millis, self(), animate),
    {noreply, State#state{running = true}}.

animate(State = #state{channel = Channel,
                       frame = Frame,
                       frame_millis = FrameMillis,
                       animator_state = AState,
                       animator_module = AMod}) ->

    erlang:send_after(FrameMillis, self(), animate),
    AState1 = AMod:animate(Frame, AState),

    {State1, TimingInfo} = avg_frame_time(State),
    Channel ! {send, info, TimingInfo},
    State1#state{animator_state = AState1}.

avg_frame_time(State = #state{window_secs = WindowSeconds,
                              prev_time = PrevTime,
                              times = PrevTimes,
                              name = Name}) ->
    {AvgTimeBin,
     CurrentTime,
     CurrentTimes}
        = avg_frame_time(WindowSeconds, PrevTime, PrevTimes),

    State1 = State#state{prev_time = CurrentTime,
                         times = CurrentTimes},
    TimingInfo = ws_anim_utils:info(#{animator => Name, avg_frame_time => AvgTimeBin}),
    {State1, TimingInfo}.

send_controls(State = #state{name = Name, channel = Channel}) ->
    Channel ! {send, control, textbox(Name, <<"frame_millis">>, State#state.frame_millis)}.

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

get_state(AMod, AState) ->
    {_, [_, _ | Fields]} = AMod:rec_info(),
    [_, _, _ | Values] = tuple_to_list(AState),
    maps:from_list(lists:zip(Fields, fix(Values))).

load_state(AMod, AState, Map) ->
    {RecordTupleSize, Fields} = AMod:rec_info(),
    [_RecName | Idxs] = lists:zip([rec_name | Fields], lists:seq(1, RecordTupleSize)),
    SetTupleElementFun =
      fun({Key, Idx}, Tuple)
            when Key /= name,
                 Key /= channel ->
              Value = maps:get(Key, Map),
              setelement(Idx, Tuple, unfix(Value));
         (_NameOrChannel, Tuple) ->
              Tuple
      end,
    lists:foldl(SetTupleElementFun, AState, Idxs).

fix(Values) ->
    [fix_(V) || V <- Values].

fix_(Tuple) when is_tuple(Tuple) ->
    [<<"tuple">> | tuple_to_list(Tuple)];
fix_(Atom) when is_atom(Atom) ->
    [<<"atom">>, atom_to_binary(Atom)];
fix_(X) ->
    X.

unfix([<<"tuple">> | Values]) ->
    list_to_tuple(Values);
unfix([<<"atom">>, Bin]) ->
    binary_to_atom(Bin);
unfix(X) ->
    X.

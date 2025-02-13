-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 100).

-record(state, {name,
                channel = undefined,
                frame = 1,
                animator_module,
                animator_state,
                window_secs = 1 :: integer(),
                prev_time :: integer,
                times = [] :: list()}).

start(Name, Module) ->
    Caller = self(),
    gen_server:start(?MODULE,
                     _Args = {Name, Caller, Module},
                     _Opts = []).

init({Name, Channel, AnimatorModule}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    CurrentTime = erlang:monotonic_time(millisecond),
    AState = AnimatorModule:init(Name, Channel),
    {ok, #state{name = Name,
                channel = Channel,
                animator_module = AnimatorModule,
                animator_state = AState,
                prev_time = CurrentTime}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State = #state{frame = Frame}) ->
    State1 = animate(State),
    {noreply, State1#state{frame = Frame + 1}};
handle_info({set, Field, Value},
            State = #state{animator_state = AState,
                           animator_module = AMod}) ->
    AState1 = AMod:set(Field, Value, AState),
    {noreply, State#state{animator_state = AState1}};
handle_info(send_controls,
            State = #state{animator_state = AState,
                           animator_module = AMod}) ->
    AState1 = AMod:send_controls(AState),
    {noreply, State#state{animator_state = AState1}}.

animate(State = #state{channel = Channel,
                       frame = Frame,
                       animator_state = AState,
                       animator_module = AMod}) ->

    erlang:send_after(?FRAME_MILLIS, self(), animate),
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
        = ws_anim_utils:avg_frame_time(WindowSeconds,
                                              PrevTime,
                                              PrevTimes),

    State1 = State#state{prev_time = CurrentTime,
                         times = CurrentTimes},
    TimingInfo = ws_anim_utils:info(#{animator => Name, avg_frame_time => AvgTimeBin}),
    {State1, TimingInfo}.

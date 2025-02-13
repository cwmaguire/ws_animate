-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {name,
                channel = undefined,
                frame = 1,
                frame_millis = 100,
                animator_module,
                animator_state,
                window_secs = 1 :: integer(),
                prev_time :: integer,
                times = [] :: list(),
                running = true}).

start(Name, Module) ->
    Caller = self(),
    gen_server:start(?MODULE,
                     _Args = {Name, Caller, Module},
                     _Opts = []).

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

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State = #state{running = false}) ->
    {noreply, State};
handle_info(animate, State = #state{frame = Frame}) ->
    State1 = animate(State),
    {noreply, State1#state{frame = Frame + 1}};
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
        = ws_anim_utils:avg_frame_time(WindowSeconds,
                                              PrevTime,
                                              PrevTimes),

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


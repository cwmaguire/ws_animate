-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/1]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 40).

-record(state, {name,
                channel = undefined,
                frame = 1}).

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
handle_info(send_controls, State = #state{}) ->
    send_controls(State),
    {noreply, State}.

animate(#state{name = Name,
               channel = Channel,
               frame = Frame}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),

    %[Channel ! {buffer, draw, square(I, J, Frame, Name)} || {I, J} <- [{0, 2}, {100, -1}, {50, -2}]].
    [Channel ! {buffer, draw, square(I, J, Frame, Name)} || {I, J} <- [{0, 2}]].

square(_I, _J, Frame, Name) ->
    W = abs(math:sin((Frame / 100))) * 300,
    H = abs(math:cos((Frame / 100))) * 300,
    X = abs(math:sin((Frame / 100))) * (800 - W),
    Y = abs(math:cos((Frame / 100))) * (700 - H),
    Map = #{type => <<"draw">>,
            cmd => <<"square">>,
            x => X,
            y => Y,
            w => W,
            h => H,
            style => <<"black">>,
            name => Name},
    ws_anim_utils:json(Map).

send_controls(_State) ->
    %% Channel ! {send, control, Json},
    ok.

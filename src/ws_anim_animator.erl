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

init({Name, WebSocket}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    {ok, #state{name = Name, channel = WebSocket}}.

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

    [Channel ! {buffer, draw, square(I, J, Frame, Name)} || {I, J} <- [{0, 2}, {100, -1}, {50, -2}]].

square(I, J, Frame, Name) ->
    Map = #{type => <<"draw">>,
            cmd => <<"square">>,
            x => I + (Frame * J),
            y => I + (Frame * J),
            w => I + Frame,
            h => I + Frame,
            style => <<"black">>,
            name => Name},
    iolist_to_binary(json:encode(Map)).

send_controls(_State) ->
    %% Channel ! {send, control, Json},
    ok.

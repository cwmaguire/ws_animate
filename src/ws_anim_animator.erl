-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-define(FRAME_MILLIS, 250).

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

handle_info(animate, State = #state{name = Name,
                                    channel = Channel,
                                    frame = Frame}) ->
    erlang:send_after(?FRAME_MILLIS, self(), animate),
    Map = #{type => <<"draw">>,
            cmd => <<"square">>,
            x => 20 + (Frame * 2),
            y => 20 + (Frame * 2),
            w => 50 + Frame,
            h => 50 + Frame,
            style => <<"black">>,
            name => Name},
    Json = iolist_to_binary(json:encode(Map)),
    Channel ! {buffer, draw, Json},
    {noreply, State#state{frame = Frame + 1}}.

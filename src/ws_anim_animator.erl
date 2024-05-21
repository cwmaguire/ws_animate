-module(ws_anim_animator).

-behaviour(gen_server).

-export([start/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).

-record(state, {name,
                channel = undefined}).

start(Name) ->
    Caller = self(),
    gen_server:start(?MODULE, _Args = {Name, Caller}, _Opts = []).

init({Name, WebSocket}) ->
    erlang:send_after(1000, self(), animate),
    {ok, #state{name = Name, channel = WebSocket}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(animate, State = #state{name = Name, channel = Channel}) ->
    erlang:send_after(1000, self(), animate),
    Channel ! {send, draw, <<"{\"animate\": \"foo\", \"name\": \"", Name/binary, "}">>},
    {noreply, State}.

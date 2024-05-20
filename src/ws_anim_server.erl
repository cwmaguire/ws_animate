-module(ws_anim_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

start_link() ->
    gen_server:start_link(ws_anim_server, _Args = undefined, _Opts = []).

init(_Args) ->
    {ok, ok}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

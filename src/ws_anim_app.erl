%%%-------------------------------------------------------------------
%% @doc ws_anim public API
%% @end
%%%-------------------------------------------------------------------

-module(ws_anim_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(cowboy),

    io:format(user, "Start~n", []),

    Dispatch = cowboy_router:compile([
        {'_', [{"/static", ws_anim_static_handler, []},
               {"/ws", ws_anim_socket, []},
               {"/", cowboy_static, {priv_file, ws_anim, "html/index.html"}},
               {"/scripts/[...]", cowboy_static, {priv_dir, ws_anim, "scripts"}},
               {"/html/[...]", cowboy_static, {priv_dir, ws_anim, "html"}}]}
    ]),

    io:format(user, "Dispatch = ~p~n", [Dispatch]),

    Result = cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format(user, "Result = ~p~n", [Result]),

    %% I don't think I need a supervisor: if a web socket's animation server crashes, the websocket can crash.
    ws_anim_sup:start_link().

stop(_State) ->
    ok.

%% internal functions

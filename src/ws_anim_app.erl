-module(ws_anim_app).

-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    application:ensure_all_started(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [{"/static", ws_anim_static_handler, []},
               {"/ws", ws_anim_socket, []},
               {"/", cowboy_static, {priv_file, ws_anim, "html/index.html"}},
               {"/scripts/[...]", cowboy_static, {priv_dir, ws_anim, "scripts"}},
               {"/html/[...]", cowboy_static, {priv_dir, ws_anim, "html"}},
               {"/images/[...]", cowboy_static, {priv_dir, ws_anim, "images"}}]}
    ]),

    cowboy:start_clear(my_http_listener,
        [{port, 8081}],
        #{env => #{dispatch => Dispatch}}
    ),

    ws_anim_sup:start_link().

stop(_State) ->
    ok.

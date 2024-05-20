-module(ws_anim_ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).

init(Req, _) ->
    {cowboy_websocket, Req, undefined}.

websocket_init(_InitState) ->
    Animator = ws_anim_server:start_link(),
    % Response can just be 'ok'
    {_Response = [{text, <<"Hello!">>}], _State = Animator}.


websocket_handle(Frame = {text, _}, Animator) ->
    io:format("Received text frame: ~n~p~n", [Frame]),
    {[Frame], Animator};
websocket_handle(Frame, Animator) ->
    io:format("Received other frame: ~n~p~n", [Frame]),
    {ok, Animator}.

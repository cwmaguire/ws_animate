-module(ws_anim_socket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {channel = undefined,
                channel_name = undefined}).

init(Req, _) ->
    {cowboy_websocket, Req, []}.

websocket_init(_InitState) ->
    {_Response = [{text, <<"Hello!">>}], #state{}}.

websocket_handle(Frame = {text, Bin}, State) ->
    io:format("Received text frame: ~n~p~n", [Frame]),
    {ReturnText, NewState} = do(Bin, State),
    io:format("Sending back ~p~n", [ReturnText]),
    {[{text, ReturnText}], NewState};
websocket_handle(Frame, Animator) ->
    io:format("Received other frame: ~n~p~n", [Frame]),
    {ok, Animator}.

websocket_info({send, Text}, State) ->
    {[{text, Text}], State};
websocket_info(Info, State) ->
    io:format("Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

do(<<"channel name">>, State = #state{channel = undefined}) ->
    Log = log(<<"Not joined to channel">>),
    {Log, State};
do(<<"channel name">>, State = #state{channel_name = ChannelName}) ->
    Log = log(<<"Joined to channel ", ChannelName/binary>>),
    {Log, State};
do(<<"channel list">>, State) ->
    Log = ws_anim_channel_registry:list(),
    {Log, State};
do(<<"channel start">>, _State) ->
    {Pid, Name} = ws_anim_channel_registry:start(),
    {Name, #state{channel = Pid,
                  channel_name = Name}};
do(<<"channel join ", Name/binary>>, State = #state{channel = undefined}) ->
    case ws_anim_channel_registry:lookup(Name) of
        undefined ->
            Log = log(<<"Could not find channel", Name/binary>>),
            {Log, State};
        Pid ->
             Log = ws_anim_channel:join(Pid),
             {Log, State#state{channel = Pid}}
    end;
do(<<"channel join ", Name/binary>>, State = #state{channel_name = Name}) ->
    Log = log(<<"Already joined to channel ", Name/binary>>),
    {Log, State};
do(<<"channel leave">>, State = #state{channel = undefined}) ->
    Log = log(<<"Not in a channel">>),
    {Log, State};
do(<<"channel leave">>, State = #state{channel = Channel}) ->
    Log = ws_anim_channel:leave(Channel),
    {Log, State#state{channel = undefined}};
do(<<"channel sub ", _/binary>>, State = #state{channel = undefined}) ->
    Log = log(<<"Not in a channel">>),
    {Log, State};
do(<<"channel sub ", Type/binary>>, State = #state{channel = Channel}) ->
    Log = ws_anim_channel:sub(Channel, Type),
    {Log, State};
do(<<"channel subs">>, State = #state{channel = Channel}) ->
    Log = ws_anim_channel:subs(Channel),
    {Log, State};
do(<<"animator add ", Animator/binary>>, State = #state{channel = Channel}) ->
    Log = ws_anim_channel:add_animator(Channel, Animator),
    {Log, State};
do(Other, State) ->
    Log = log(<<"Command '", Other/binary, "' not recognized">>),
    {Log, State}.

log(Bin) ->
    <<"{\"log\": \"", Bin/binary, "\"}">>.

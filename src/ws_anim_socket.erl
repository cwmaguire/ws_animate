-module(ws_anim_socket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {channel = undefined,
                channel_name = undefined}).

init(Req, _) ->
    {cowboy_websocket, Req, [], #{idle_timeout => infinity}}.

websocket_init(_InitState) ->
    Log = ws_anim_utils:log(<<"Connected">>),
    {_Response = [{text, Log}], #state{}}.

websocket_handle(Frame = {text, Bin}, State) ->
    io:format("Received text frame: ~n~p: ~p~n", [self(), Frame]),
    {Msgs, NewState} = do(Bin, State),
    %io:format("Sending back ~p~n", [ReturnText]),
    Return = [{text, Msg} || Msg <- Msgs],
    {Return, NewState};
websocket_handle(Frame, State) ->
    io:format("Received other frame: ~n~p: ~p~n", [self(), Frame]),
    {ok, State}.

websocket_info({send, Text}, State) ->
    {[{text, Text}], State};
websocket_info(Info, State) ->
    io:format("Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

do(<<"channel name">>, State = #state{channel = undefined}) ->
    Msg = ws_anim_utils:log(<<"Not joined to channel">>),
    {[Msg], State};
do(<<"channel name">>, State = #state{channel_name = ChannelName}) ->
    Log = ws_anim_utils:log(<<"Joined to channel ", ChannelName/binary>>),
    {[Log], State};
do(<<"channel list">>, State) ->
    Msg = ws_anim_channel_registry:list(),
    {[Msg], State};
do(<<"channel start">>, _State) ->
    {Pid, Name} = ws_anim_channel_registry:start(),
    Info = ws_anim_utils:info(#{channel_name => Name}),
    Log = ws_anim_utils:log(<<"Channel name is ", Name/binary>>),
    {[Info, Log], #state{channel = Pid,
                         channel_name = Name}};
do(<<"channel join ", Name/binary>>, State = #state{channel = undefined}) ->
    case ws_anim_channel_registry:lookup(Name) of
        undefined ->
            Log = ws_anim_utils:log(<<"Could not find channel", Name/binary>>),
            {[Log], State};
        Pid ->
             Log = ws_anim_channel:join(Pid),
             {[Log], State#state{channel = Pid}}
    end;
do(<<"channel join ", Name/binary>>, State = #state{channel_name = Name}) ->
    Log = ws_anim_utils:log(<<"Already joined to channel ", Name/binary>>),
    {[Log], State};
do(<<"channel leave">>, State = #state{channel = undefined}) ->
    Log = ws_anim_utils:log(<<"Not in a channel">>),
    {[Log], State};
do(<<"channel leave">>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:leave(Channel),
    {[Msg], State#state{channel = undefined}};
do(<<"channel sub ", _/binary>>, State = #state{channel = undefined}) ->
    Log = ws_anim_utils:log(<<"Not in a channel">>),
    {[Log], State};
do(<<"channel sub ", Type/binary>>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:sub(Channel, Type),
    {[Msg], State};
do(<<"channel subs">>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:subs(Channel),
    {[Msg], State};
do(<<"channel save ", Filename/binary>>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:save(Channel, Filename),
    {[Msg], State};
do(<<"channel load ", Filename/binary>>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:load(Channel, Filename),
    {[Msg], State};
do(<<"animator list">>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:animator_list(Channel),
    {[Msg], State};
do(<<"animator add ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msg = ws_anim_channel:add_animator(Channel, Animator),
    {[Msg], State};
do(<<"animator set ", AnimatorFieldValue/binary>>, State = #state{channel = Channel}) ->
    io:format(user, "AnimatorFieldValue = ~p~n", [AnimatorFieldValue]),
    Log = ws_anim_channel:animator_set_field_value(Channel, AnimatorFieldValue),
    {[Log], State};
do(<<"animator stop ", Animator/binary>>, State = #state{channel = Channel}) ->
    io:format(user, "Stop animator ~p~n", [Animator]),
    Log = ws_anim_channel:animator_stop(Channel, Animator),
    {[Log], State};
do(<<"animator start ", Animator/binary>>, State = #state{channel = Channel}) ->
    io:format(user, "Start animator ~p~n", [Animator]),
    Log = ws_anim_channel:animator_start(Channel, Animator),
    {[Log], State};
do(<<"animator freeze ", Animator/binary>>, State = #state{channel = Channel}) ->
    io:format(user, "Freeze animator ~p~n", [Animator]),
    Log = ws_anim_channel:animator_freeze(Channel, Animator),
    {[Log], State};
do(<<"animator unfreeze ", Animator/binary>>, State = #state{channel = Channel}) ->
    io:format(user, "Unfreeze animator ~p~n", [Animator]),
    Log = ws_anim_channel:animator_unfreeze(Channel, Animator),
    {[Log], State};
do(Other, State) ->
    Log = ws_anim_utils:log(<<"Command '", Other/binary, "' not recognized">>),
    {[Log], State}.

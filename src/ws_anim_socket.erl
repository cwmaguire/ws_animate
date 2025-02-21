-module(ws_anim_socket).

-include("ws_anim.hrl").

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-record(state, {channel = undefined,
                channel_name = undefined}).

init(Req, _) ->
    {cowboy_websocket, Req, [], #{idle_timeout => infinity}}.

websocket_init(_InitState) ->
    Log = ?utils:log(<<"Connected">>),
    {_Response = [{text, Log}], #state{}}.

websocket_handle({text, Bin}, State) ->
    {Msgs, NewState} = do(Bin, State),
    case Msgs of
        List when is_list(List) ->
            Return = [{text, Msg} || Msg <- Msgs],
            {Return, NewState};
        _ ->
            io:format("ws_anim_socket:websocket_handle({text, ~p}, State) returned non-list:~n    ~p~n", [Bin, Msgs]),
            {[], NewState}
    end;
websocket_handle(Frame, State) ->
    io:format("Received other frame: ~n~p: ~p~n", [self(), Frame]),
    {ok, State}.

websocket_info({send, Text}, State) ->
    {[{text, Text}], State};
websocket_info(Info, State) ->
    io:format("Socket: Received erlang message: ~n~p~n", [Info]),
    {ok, State}.

do(<<"registry sub channels">>, State) ->
    Msgs = ws_anim_channel_registry:sub(<<"channels">>),
    {Msgs, State};
do(<<"channel name">>, State = #state{channel = undefined}) ->
    Log = ?utils:log(<<"Not joined to channel">>),
    {[Log], State};
do(<<"channel name">>, State = #state{channel_name = ChannelName}) ->
    Log = ?utils:log(<<"Joined to channel ", ChannelName/binary>>),
    {[Log], State};
do(<<"channel list">>, State) ->
    Msgs = ws_anim_channel_registry:list(),
    {Msgs, State};
do(<<"channel start">>, _State) ->
    {Pid, Name} = ws_anim_channel_registry:start(),
    Info = ?utils:info(#{channel_name => Name}),
    Log = ?utils:log(<<"Channel name is ", Name/binary>>),
    {[Log, Info], #state{channel = Pid,
                         channel_name = Name}};
do(<<"channel join ", Name/binary>>, State = #state{channel = undefined}) ->
    case ws_anim_channel_registry:lookup(Name) of
        undefined ->
            Log = ?utils:log(<<"Could not find channel", Name/binary>>),
            {[Log], State};
        Pid ->
             Msgs = ws_anim_channel:join(Pid),
             {Msgs, State#state{channel = Pid}}
    end;
do(<<"channel join ", Name/binary>>, State = #state{channel_name = Name}) ->
    Log = ?utils:log(<<"Already joined to channel ", Name/binary>>),
    {[Log], State};
do(<<"channel leave">>, State = #state{channel = undefined}) ->
    Log = ?utils:log(<<"Not in a channel">>),
    {[Log], State};
do(<<"channel leave">>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:leave(Channel),
    {Msgs, State#state{channel = undefined}};
do(<<"channel set ", FieldAndValue/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:set_value(Channel, FieldAndValue),
    {Msgs, State};
do(<<"channel sub ", _/binary>>, State = #state{channel = undefined}) ->
    Log = ?utils:log(<<"Not in a channel">>),
    {[Log], State};
do(<<"channel sub ", Type/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:sub(Channel, Type),
    {Msgs, State};
do(<<"channel subs">>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:subs(Channel),
    {Msgs, State};
do(<<"channel save ", Filename/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:save(Channel, Filename),
    {Msgs, State};
do(<<"channel load ", Filename/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:load(Channel, Filename),
    {Msgs, State};
do(<<"animator list">>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_list(Channel),
    {Msgs, State};
do(<<"animator add ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:add_animator(Channel, Animator),
    {Msgs, State};
do(<<"animator set ", AnimatorFieldValue/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_set_field_value(Channel, AnimatorFieldValue),
    {Msgs, State};
do(<<"animator stop ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_stop(Channel, Animator),
    {Msgs, State};
do(<<"animator start ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_start(Channel, Animator),
    {Msgs, State};
do(<<"animator sub ", AnimatorAndType/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:sub(Channel, AnimatorAndType),
    {Msgs, State};
do(<<"animator freeze ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_freeze(Channel, Animator),
    {Msgs, State};
do(<<"animator unfreeze ", Animator/binary>>, State = #state{channel = Channel}) ->
    Msgs = ws_anim_channel:animator_unfreeze(Channel, Animator),
    {Msgs, State};
do(Other, State) ->
    Log = ?utils:log(<<"Command '", Other/binary, "' not recognized">>),
    {[Log], State}.
